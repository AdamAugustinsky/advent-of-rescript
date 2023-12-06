let input = Node.Fs.readFileSync("./src/day3/input", #utf8)
let lines = String.split(input, "\n")

//replace all non-numeric characters with nothing
module Day3Part1 = {
  let res = Array.reduceWithIndex(lines, 0, (parts_acc, line, line_index) => {
    let regex_numbers = %re("/[0-9]+/g")
    let regex_symbols = %re("/[^\\.0-9]+/g")

    let numbers_in_line = String.match(line, regex_numbers)->Option.getOr([])

    let last_number_index = ref(0)
    parts_acc +
    Array.reduce(numbers_in_line, 0, (line_acc, n) => {
      let number_index = String.indexOfFrom(line, n, last_number_index.contents)

      // stores last number index to avoid finding the first number on cases of duplicates
      last_number_index.contents = number_index + String.length(n)
      let previous_line = Array.at(lines, line_index - 1)->Option.getOr("")
      let next_line = Array.at(lines, line_index + 1)->Option.getOr("")

      let start = number_index - 1
      let end = number_index + String.length(n) + 1
      let above = String.substring(previous_line, ~start, ~end)
      let aside = String.substring(line, ~start, ~end)
      let below = String.substring(next_line, ~start, ~end)

      let has_symbol = [above, aside, below]->Array.reduce(
        false,
        (acc, line) => {
          let has_symbol = String.match(line, regex_symbols)->Option.getOr([])->Array.length > 0
          acc || has_symbol
        },
      )

      has_symbol ? line_acc + Option.getOr(Int.fromString(n), 0) : line_acc
    })
  })
}

module Day3Part2 = {
  type gear_part = {
    gear_pos: (int, int),
    num: int,
  }

  let search_gear_parts = (
    lines: array<string>,
    ~x_range: (int, int),
    ~y_range: (int, int),
    num: string,
  ) => {
    let (x_min, x_max) = x_range
    let (y_min, y_max) = y_range

    Array.slice(lines, ~start=y_min, ~end=y_max + 1)->Array.reduceWithIndex(None, (
      acc,
      line,
      line_index,
    ) => {
      let num_surroudings = String.substring(line, ~start=x_min, ~end=x_max)
      let find_star = String.searchOpt(num_surroudings, %re("/\*/g"))

      let gear_part = Option.map(find_star, x => {
        gear_pos: (y_min + line_index, x_min + x),
        num: num->Int.fromString->Option.getOr(0),
      })

      Option.orElse(gear_part, acc)
    })
  }

  let regex_numbers = %re("/[0-9]+/g")
  let gear_parts = Array.mapWithIndex(lines, (line, line_index) => {
    let last_number_index = ref(0)
    String.match(line, regex_numbers)
    ->Option.getOr([])
    ->Array.map(n => {
      let number_index = String.indexOfFrom(line, n, last_number_index.contents)

      // stores last number index to avoid finding the first number on cases of duplicates
      last_number_index := number_index + String.length(n)
      let x_min = number_index > 0 ? number_index - 1 : number_index
      let x_max = number_index + String.length(n) + 1

      search_gear_parts(
        lines,
        ~x_range=(x_min, x_max),
        ~y_range=(line_index > 0 ? line_index - 1 : line_index, line_index + 1),
        n,
      )
    })
    ->Array.keepSome
  })->Array.flat

  let rec sum_gear_powers = (gear_parts, acc) => {
    let opt_gear = gear_parts[0]

    Option.mapOr(opt_gear, acc, gear =>
      gear_parts
      ->Array.findIndexOpt(g => g.gear_pos == gear.gear_pos && g.num !== gear.num)
      ->Option.mapOr(acc, pair_index => {
        let gear_power = gear_parts[pair_index]->Option.mapOr(0, pair => gear.num * pair.num)

        let parts_without_current_gear = Array.filter(
          gear_parts,
          x => Array.indexOf(gear_parts, x) !== pair_index && x !== gear,
        )

        sum_gear_powers(parts_without_current_gear, acc + gear_power)
      })
    )
  }
  let res = sum_gear_powers(gear_parts, 0)
}

Console.log(Day3Part2.res)
