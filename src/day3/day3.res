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

    lines
    ->Array.slice(~start=y_min, ~end=y_max + 1)
    ->Array.reduceWithIndex(None, (acc, line, line_index) =>
      String.substring(line, ~start=x_min, ~end=x_max)
      ->String.searchOpt(%re("/\*/g"))
      ->Option.map(x => {
        gear_pos: (line_index + y_min, x_min + x),
        num: num->Int.fromString->Option.getOr(0),
      })
      ->Option.orElse(acc)
    )
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

      let y_min = line_index > 0 ? line_index - 1 : line_index
      let y_max = line_index + 1

      search_gear_parts(lines, ~x_range=(x_min, x_max), ~y_range=(y_min, y_max), n)
    })
    ->Array.keepSome
  })->Array.flat

  let find_index_to_opt = i =>
    switch i {
    | -1 => None
    | i => Some(i)
    }
  let rec sum_gear_powers = (~acc=0, gear_parts) => {
    let current_gear = Array.at(gear_parts, 0)

    Option.mapOr(current_gear, acc, gear => {
      let pair_index_opt =
        gear_parts
        ->Array.findIndexWithIndex((g, g_index) => g.gear_pos == gear.gear_pos && g_index !== 0)
        ->find_index_to_opt

      let gear_power = Option.mapOr(pair_index_opt, 0, pair_index =>
        Array.at(gear_parts, pair_index)->Option.mapOr(0, pair => gear.num * pair.num)
      )

      // pair is set to 0 if not present, because 0 will be removed anyway
      let gear_pair_to_remove = Option.mapOr(pair_index_opt, 0, pair_index => pair_index)

      let parts_without_current_gear = Array.filterWithIndex(gear_parts, (_, i) =>
        // removes current gear and its pair
        i !== gear_pair_to_remove && i !== 0
      )

      sum_gear_powers(~acc=acc + gear_power, parts_without_current_gear)
    })
  }
  let res = sum_gear_powers(gear_parts)
}

Console.log(Day3Part2.res)
