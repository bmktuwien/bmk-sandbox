import           Libnotify

main :: IO ()
main = do
  display_ $
       summary "Hello!"
    <> body "Nadal just won !!!"
    <> icon "dialog-information"
    <> timeout Default
    <> icon "face-embarrassed"
