
bodyfatpercentage <- function(Age, Weight, Height, Neck, Chest, Abdomen, Hip, Thigh, Knee, Ankle, Biceps, Forearm, Wrist) {
  # your code goes here
  bodyfat <- -346.030186086169 + 208.728057539987 * log10(Abdomen)+-0.0992285090208676 * ceiling(Weight)+-1.47439740274781 * Wrist
  
  return (bodyfat)
}

# bodyfatpercentage(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)