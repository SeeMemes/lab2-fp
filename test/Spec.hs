import Test.Hspec
import Test.QuickCheck
import Trie

main :: IO ()
main = hspec $
  describe "dictionary_trie_tree" $ do
    it "fork" $ do
      (member "hottestest" $ insert "hottestest" $ dictionary) `shouldBe` True
      (member "saddest" $ insert "saddest" $ dictionary) `shouldBe` True

    it "forked" $ do
      (member "hottest" $ delete "hotter"  $ dictionary) `shouldBe` True
      (member "hotter"  $ delete "hottest" $ dictionary) `shouldBe` True
      (member "hottest" $ delete "hottest" $ dictionary) `shouldBe` False
      (member "hotter"  $ delete "hotter"  $ dictionary) `shouldBe` False

    it "inlined" $ do
      (member "h" $ insert "h" $ dictionary) `shouldBe` True
      (member "H" $ insert "h" $ dictionary) `shouldBe` False

    it "inline" $ do
      (member "help"   $ delete "helper" $ dictionary) `shouldBe` True
      (member "helper" $ delete "help"   $ dictionary) `shouldBe` True
      (member "sad"    $ delete "said"   $ dictionary) `shouldBe` True
      (member "said"   $ delete "sad"    $ dictionary) `shouldBe` True
      (member "p"      $ delete "pi"     $ dictionary) `shouldBe` True
      (member "pi"     $ delete "p"      $ dictionary) `shouldBe` True
      (member "help"   $ delete "help"   $ dictionary) `shouldBe` False
      (member "helper" $ delete "helper" $ dictionary) `shouldBe` False
      (member "heat"   $ delete "heat"   $ dictionary) `shouldBe` False
      (member "sad"    $ delete "sad"    $ dictionary) `shouldBe` False
      (member "said"   $ delete "said"   $ dictionary) `shouldBe` False
      (member "p"      $ delete "p"      $ dictionary) `shouldBe` False
      (member "pi"     $ delete "pi"     $ dictionary) `shouldBe` False

    it "separate paths" $ do
      (member "bad"  $ delete "good" $ dictionary) `shouldBe` True
      (member "good" $ delete "bad"  $ dictionary) `shouldBe` True
      (member "bad"  $ delete "bad"  $ dictionary) `shouldBe` False
      (member "good" $ delete "good" $ dictionary) `shouldBe` False