import qualified Test.DocTest as Doctest

main :: IO ()
main = Doctest.doctest ["src/lib"]
