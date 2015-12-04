module Esercitazione0

let NomeCognome : string = "Federico Longhin"

let euclides_ipotenusa (cathetus : float) (projection : float) : float =
    if cathetus <> 0.0 || projection <> 0.0
    then
        (cathetus * cathetus) / projection
    else 0.0;;

let segment_integral (x1 : float) (y1 : float) (x2 : float) (y2 : float)  : float =
    if x1 <> 0.0 || y1 <> 0.0 || x2 <> 0.0 || y2 <> 0.0
    then
        0.5 * abs(x2 - x1) * (y1 + y2)
    else 0.0;;

let triangle_area (cathetus : float) (projection : float) : float =
    if cathetus <> 0.0 || projection <> 0.0
    then
        let hypotenuse = euclides_ipotenusa cathetus projection
        let height = sqrt(cathetus**2.0 - projection**2.0)
        (hypotenuse * height) / 2.0
    else 0.0;;