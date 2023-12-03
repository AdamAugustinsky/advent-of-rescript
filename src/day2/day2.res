module Part1 = {
  let input = Node.Fs.readFileSync("./src/day2/input", #utf8)
  let lines = String.split(input, "\n")

  let max_red = 12
  let max_green = 13
  let max_blue = 14

  let sum_boxes = boxes =>
    Array.reduce(boxes, 0, (acc, box) => {
      let box = String.split(String.trim(box), " ")
      acc + Option.getOr(Int.fromString(Option.getOr(box[0], "")), 0)
    })

  let game_ids_sum = Array.reduce(lines, 0, (acc, line) => {
    let split_line = String.split(line, ":")
    let game_id = String.replace(Option.getExn(split_line[0]), "Game ", "")

    let boxes =
      Option.getOr(split_line[1], "")->String.replaceRegExp(%re("/;/g"), ",")->String.split(",")

    let valid_boxes = Array.map(boxes, box => {
      let play = String.split(box, ",")
      let green_sum = sum_boxes(Array.filter(play, p => String.includes(p, "green")))
      let blue_sum = sum_boxes(Array.filter(play, p => String.includes(p, "blue")))
      let red_sum = sum_boxes(Array.filter(play, p => String.includes(p, "red")))

      if green_sum > max_green || blue_sum > max_blue || red_sum > max_red {
        false
      } else {
        true
      }
    })

    Array.reduce(valid_boxes, true, (acc, valid) => acc && valid)
      ? acc + Option.getOr(Int.fromString(game_id), 0)
      : acc + 0
  })
}
module Part2 = {
  let input = Node.Fs.readFileSync("./src/day2/input", #utf8)
  let lines = String.split(input, "\n")

  let get_max_from_boxes = boxes => {
    let num_from_box = Array.map(boxes, box =>
      box->String.trim(_)->String.split(" ")->Array.at(0)->Option.getOr("")->Int.fromString->Option.getOr(0)
    )

    Array.reduce(num_from_box, 0, (previous, current) => current > previous ? current : previous)
  }

  let games_power = Array.reduce(lines, 0, (acc, line) => {
    let split_line = String.split(line, ":")

    let boxes =
      Option.getOr(split_line[1], "")->String.replaceRegExp(%re("/;/g"), ",")->String.split(",")

    let max_green = Array.filter(boxes, String.includes(_, "green"))->get_max_from_boxes
    let max_blue = Array.filter(boxes, String.includes(_, "blue"))->get_max_from_boxes
    let max_red = Array.filter(boxes, String.includes(_, "red"))->get_max_from_boxes
    acc + max_green * max_blue * max_red
  })
}

Js.log2("part 1", Part1.game_ids_sum)
Js.log2("part 2", Part2.games_power)
