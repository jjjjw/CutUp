module FoldIn(foldIn) where


fold :: String -> ([String], [String])
fold s = bisected
  where
    page = lines s
    midpoint = div (length page) 2
    bisected = splitAt midpoint page


foldIn :: String -> String -> String
foldIn s1 s2 = unlines spliced
  where
    spliced = (fst (fold s1)) ++ (snd (fold s2))


main =
  print (foldIn "The sky above the port was the color of television, tuned to a dead channel. \n It's not like I'm using,\" Case heard someone say, as he shouldered his way through the crowd around the door of the Chat. \n \"It's like my body's developed this massive drug deficiency.\"It was a Sprawl voice and a Sprawl joke. \n The Chatsubo was a bar for professional expatriates; you could drink there for a week and never hear two words in Japanese."
    "Call me Ishmael. \n Some years ago - never mind how long precisely - having little or no money in my purse, and nothing particular to interest me on shore, \n I thought I would sail about a little and see the watery part of the world.")
