let run ~eq f start = 
  let rec iter x = 
    let y = f x in
    if eq y x then x else iter y 
  in
  iter start

