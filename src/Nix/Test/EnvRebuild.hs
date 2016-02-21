{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards#-}
    
module Main where
       
import BasicPrelude hiding ((</>), (<.>), FilePath)
import Filesystem.Path.CurrentOS
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Error
import Control.Lens

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Attoparsec.Text

import Nix.Packages
import Nix.Commands
import Utils
import qualified Nix.OutputParser as P
---------------------------
-- Tests
---------------------------
                 
ex_someInstalling :: [Text]
ex_someInstalling =
  [ "installing `rxvt-unicode-9.16-with-perl'"
  , "  installing `subversion-1.7.13'"
  , "installing `texlive-full'"
  ]
  
ex_someInstalling2 :: [Text]
ex_someInstalling2 =
  [ "installing `rxvt-unicode-9.16-with-perl'"
  , "  installing `subversion-1.7.13'"
  , "these derivations will be built:"
  , "  /nix/store/4w40vbz4cix0z474shpcnmfxjc7kh69z-texlive-core-2014.drv"
  , "installing `texlive-full'"
  ]

ex_someUninstalling :: [Text]
ex_someUninstalling =
  [ "uninstalling `texlive-full'"
  , "  uninstalling `aspell-0.60.6.1'"
  , "uninstalling `aspell-dict-de-20030222-1'"
  , "uninstalling `aspell-dict-en-7.1-0'"
  ]

ex_someBuilding :: [Text]
ex_someBuilding =
  [ "/nix/store/njrzm1kkj9k22vr7xaggwjmz6cm7nsxv-haskell-env-ghc-7.6.3.drv"
  ] 

ex_someFetching :: [Text] 
ex_someFetching = 
  [ "  /nix/store/h3q083n3yaap9vhcsd1hlrkcs8qph7fb-clucene-core-2.3.3.4"
  , "  /nix/store/hb2c7hbsxksgzl50n35dw42bp9z389s6-parcellite-1.1.6"
  , "  /nix/store/hcz4gl5nhchawv4b162xmcnn4gqw9z49-alex-3.0.5"
  ] 

test_someInstalling = testCase "installing" $
  rights (fmap (parseOnly P.fromInstalling) ex_someInstalling)
  @=?
  ["rxvt-unicode-9.16-with-perl","subversion-1.7.13","texlive-full"]

test_someInstalling2 = testCase "installing2" $
  rights (fmap (parseOnly P.fromInstalling) ex_someInstalling2)
  @=?
  ["rxvt-unicode-9.16-with-perl","subversion-1.7.13","texlive-full"]
  
test_someUninstalling = testCase "uninstalling" $
  rights (fmap (parseOnly P.fromUninstalling) ex_someUninstalling)
  @=?
  [ "texlive-full"
  , "aspell-0.60.6.1"
  , "aspell-dict-de-20030222-1"
  , "aspell-dict-en-7.1-0"
  ]

test_someBuilding = testCase "building" $
  rights (fmap (parseOnly p_fromBuilding) ex_someBuilding)
  @=?
  ["haskell-env-ghc-7.6.3"]

  
test_someFetched = testCase "fetching" $
  rights (fmap (parseOnly p_fromFetching) ex_someFetching)
  @=?
  [ "clucene-core-2.3.3.4"
  , "parcellite-1.1.6"
  , "alex-3.0.5"
  ] 


main :: IO ()
main = defaultMain
  [test_someInstalling
  , test_someBuilding
  , test_someUninstalling
  , test_someFetched
  , testGroup "successfull parses" $
    [ testCase "local" $
       parseOnly P.fromLocalQuery "clucene-core-2.3.3.4 /nix/store/bla"
       @?=
       Right (Just ("clucene-core-2.3.3.4", "/nix/store/bla", Present))
  
    , testCase "remote prebuilt" $
       parseOnly P.fromRemoteQuery "--S clucene-core-2.3.3.4 /nix/store/bla"
       @?=
       Right (Just ("clucene-core-2.3.3.4", "/nix/store/bla", Prebuilt))

    , testCase "remote source" $
       parseOnly P.fromRemoteQuery "--- clucene-core-2.3.3.4 /nix/store/bla"
       @?=
       Right (Just ("clucene-core-2.3.3.4", "/nix/store/bla", Source))
    ]
  , testGroup "failing parses" $
    [ testCase "missing path" $
        (length . lefts . map (parseOnly P.fromQuery) 
          $ ["texlive-full   ", "texlive-full"])
        @?= 2
    , testCase "parse local with status" $
       isLeft (parseOnly P.fromLocalQuery "--- clucene-core-2.3.3.4 /nix/store/bla")
       @?=
       True
    ]
  , testGroup "commands" $
    [ testCase "install declared packages into profile" $
      nixCmdStrings
      (Nix { nixCommand =   (NixInstall (NIOs False Nothing))
           , nixSelection = Nothing
           , nixDryRun =    True
           , nixProfile =   ("some" </> "profile")
           , nixFile =      (Just $ "somedir" </> "declared-packages.nix")
           , nixInclude =   Nothing
           })
      @?=
      ("nix-env", [ "--dry-run"
                  , "--profile", "some/profile"
                  , "--file", "somedir/declared-packages.nix"
                  , "--install"
                  , "*"
                  ])

    , testCase "install selection of declared packages into profile" $
      nixCmdStrings
      (Nix { nixCommand =   (NixInstall (NIOs False Nothing))
           , nixSelection = (Just ["a", "b", "c"])
           , nixDryRun =    True
           , nixProfile =   ("some" </> "profile")
           , nixFile =      (Just $ "somedir" </> "declared-packages.nix")
           , nixInclude =   Nothing
           })
      @?=
      ("nix-env", [ "--dry-run"
                  , "--profile", "some/profile"
                  , "--file", "somedir/declared-packages.nix"
                  , "--install"
                  , "a"
                  , "b"
                  , "c"
                  ])

    , testCase "install all packages from profile into profile" $
      over _2 S.fromList (nixCmdStrings
      (Nix { nixCommand =   (NixInstall (NIOs False (Just $ "source" </> "profile")))
           , nixSelection = Nothing
           , nixDryRun =    True
           , nixProfile =   ("some" </> "profile")
           , nixFile =      Nothing
           , nixInclude =   Nothing
           }))
      @?=
      ("nix-env", S.fromList [ "--dry-run"
                  , "--profile", "some/profile"
                  , "--install"
                  , "--from-profile", "source/profile"
                  , "*"
                  ])
    , testCase "remove all packages from a profile" $
      nixCmdStrings
      (Nix { nixCommand =   (NixUninstall)
           , nixSelection = Nothing
           , nixDryRun =    False
           , nixProfile =   ("some" </> "profile")
           , nixFile =      Nothing
           , nixInclude =   Nothing
           })
      @?=
      ("nix-env", [ "--profile", "some/profile", "-e", "*"])
      
    ]

  , testGroup "update"
    [ 
      testCase "match1" $
      findUpdate' "cabal2nix-1.60" "cabal2nix-1.58" 
      @?= Just (mkUpd ("cabal2nix", "1.58", "1.60"))

    , testCase "match2" $
      findUpdate' "git-full-1.9.0 /nix/store/bla1" "git-1.8.5.2-full /nix/store/bla2" @?= Nothing

    , testCase "match3" $
      findUpdate' "git-annex-5.20140306" "git-annex-5.20140108" 
      @?= Just (mkUpd ("git-annex", "5.20140108", "5.20140306"))

    , testCase "no match" $
      findUpdate' "git-1.8.5.2-full" "giti-full-1.9.0" @?= Nothing

    , testCase "no match, same prefix" $
      findUpdate' "git-1.9.4"  "git-annex-5.20140717" @?= Nothing

    , testCase "filterUpd" $
      calculateUpdatesFromLists 
                    [ "cabal2nix-1.60"
                    , "duplicity-0.6.23"
                    , "feh-2.10"
                    , "git-annex-5.20140306"
                    ]
                   [ "git-annex-5.20140108"
                   , "cabal-dev-0.9.2"
                   , "cabal2nix-1.58"
                   , "duplicity-0.6.22"
                   , "exif-0.6.21" 
                   ]
      @?= expectedUpdates 
            [ ("cabal2nix", "1.58", "1.60")
              , ("duplicity", "0.6.22", "0.6.23")
              , ("git-annex", "5.20140108", "5.20140306")
            ]
           ["feh-2.10"]
           [ "cabal-dev-0.9.2"
            , "exif-0.6.21"
            ]

    , testCase "filterUpd_same_prefixes" $
      calculateUpdatesFromLists 
                    [ "cabal2nix-1.60"
                    , "duplicity-0.6.23"
                    , "feh-2.10"
                    , "git-2.0"
                    , "git-annex-5.20140306"
                    ]
                   [ "git-annex-5.20140108"
                   , "cabal-dev-0.9.2"
                   , "cabal2nix-1.58"
                   , "duplicity-0.6.22"
                    , "git-1.8"
                   , "exif-0.6.21" 
                   ]
      @?= expectedUpdates
            [   ("cabal2nix", "1.58" ,"1.60")
              , ("duplicity", "0.6.22" ,"0.6.23")
              , ("git-annex", "5.20140108", "5.20140306")
              , ("git", "1.8", "2.0")
            ]
            ["feh-2.10"]
            [ "cabal-dev-0.9.2" , "exif-0.6.21"]
     , testCase "unversioned" $
       let texliveOld = packageWithPathFromText "texlive-full /nix/store/bla1"
           texliveNew = packageWithPathFromText "texlive-full /nix/store/bla2"
       in (view _1 $ calculateUpdates (M.fromList [texliveNew])
                                (S.fromList [fst texliveOld])
          ) @?= S.fromList [ Upd { uName = "texlive-full" 
                                 , uOld = "" 
                                 , uOldPath = "/nix/store/bla1"           
                                 , uNew = ""                                  
                                 , uNewPath = "/nix/store/bla2"
                                 , uStatus = Present
                                 }
                            ]
     ]

  , testGroup "Result views"
    [ testCase "removing" $
      removing ex_result1 @?= fromPackageListWithStatus ["remove-me-1", "somepackage-1.1.2"]

    , testCase "install fresh" $
      installing ex_result1 @?= fromPackageListWithStatus ["newpackage-1", "somepackage-1.1.1"]

    , testCase "wantedFromDeclared" $
      wantedFromDeclared ex_result1 
      @?= fromPackageListWithStatus["newpackage-1", "somepackage-1.1.1"]

    , testCase "unversioned updates" $
      installing ex_texliveResults
      @?= M.fromList 
          [ (Pwp { pwpPkg =  VPkg { pName = "texlive-full" , pVer = ""}
                , pwpPath = "/nix/store/lynr5fvcpp21rzjaz1ahjzn1zd7r0dkr-TeXLive-linkdir"
                }, Present)
          ]
  
    , testCase "unversioned blocked updates" $
      blockedUpdates (ex_texliveResults { rStorePaths = M.keysSet (rInstalled ex_texliveResults) })
      @?= S.fromList
          [ Upd { uName =  "texlive-full" 
                , uOld = ""
                , uNew = ""
                , uOldPath = "/nix/store/4lyy252ablh9snx5cr4dvfic0k0fcr0y-TeXLive-linkdir"
                , uNewPath = "/nix/store/lynr5fvcpp21rzjaz1ahjzn1zd7r0dkr-TeXLive-linkdir"
                , uStatus = Present
                }
          ]
   ]
 ]

ex_result1 = Results { rStorePaths = S.fromList $ map (mkOld . parseVersionedPackage)
                               [ "libreoffice-2.3"
                               , "Agda-3.4"
                               , "emacs-24"
                               , "not-wanted-222"
                               ]
                     , rDeclared = M.fromList $ map (addPresentStatus . mkOld . parseVersionedPackage)
                                   [ "libreoffice-2.4"
                                   , "somepackage-1.1.1"
                                   , "newpackage-1"
                                   , "Agda-3.4"
                                   , "emacs-25"
                                   ]
                     , rInstalled = M.fromList $ map (addPresentStatus . mkOld . parseVersionedPackage)
                                    [ "libreoffice-2.4"
                                    , "somepackage-1.1.2"
                                    , "remove-me-1"
                                    ]
                     }
ex_texliveResults = Results { rStorePaths = S.empty
                          , rDeclared  = M.fromList 
                                         . map (packageWithPathFromText)
                                         $ [ "texlive-full  /nix/store/lynr5fvcpp21rzjaz1ahjzn1zd7r0dkr-TeXLive-linkdir"
                                           ]
                          , rInstalled  = M.fromList 
                                         . map (packageWithPathFromText)
                                         $ [ "texlive-full  /nix/store/4lyy252ablh9snx5cr4dvfic0k0fcr0y-TeXLive-linkdir"
                                           ]
                          }

  
findUpdate' p1 p2 = findUpdate (mkNew . parseVersionedPackage $ p1) 
                               (mkOld . parseVersionedPackage $ p2)

fromPackageListWithStatus ps = M.fromList . map (addPresentStatus . mkOld . parseVersionedPackage) $ ps
fromPackageList ps = S.fromList . map (mkOld . parseVersionedPackage) $ ps
packageWithPathFromText s = 
  fromJustE ("packageWithPathFromText: no parse of "<>s) 
  . preview (_Right._Just) 
  . P.parsePackageWithPath P.fromLocalQuery 
  $ s 

mkUpd (uName, uOld, uNew) = Upd { uName, uOld, uNew
                            , uOldPath = "/nix/store/bl1"
                            , uNewPath = "/nix/store/bl2" 
                            , uStatus = Present }
mkOld vpkg = Pwp { pwpPkg = vpkg, pwpPath = "/nix/store/bl1"}
mkNew vpkg = (Pwp { pwpPkg = vpkg, pwpPath = "/nix/store/bl2"} , Present )

mkNewPackages = M.fromList . map (mkNew . parseVersionedPackage)
mkOldPackages = S.fromList . map (mkOld . parseVersionedPackage)

p_fromBuilding = P.fromBuilding "/nix/store"
p_fromFetching = P.fromFetching "/nix/store"

calculateUpdatesFromLists news olds = 
  calculateUpdates (mkNewPackages news)
                   (mkOldPackages olds)

expectedUpdates :: [(Text, Text, Text)] -> [Text] -> [Text]
                -> (Set Upd, Map PackageWithPath PkgStatus, Set PackageWithPath)
expectedUpdates upds added removed =
  ( S.fromList . map mkUpd $ upds
  , mkNewPackages added
  , mkOldPackages removed )

addPresentStatus p = (p, Present)
