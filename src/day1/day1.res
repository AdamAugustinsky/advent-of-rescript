let input = Node.Fs.readFileSync("./src/day1/input", #utf8)
let lines = String.split(input, "\n")

//replace all non-numeric characters with nothing
let find_first = (str: string, ~reverse: bool) => {
  let rec aux = (str: string, ~start: int, ~pointer: int, ~reverse) => {
    let end_of_loop = reverse ? 0 : String.length(str)
    let increment = reverse ? -1 : 1
    let substring = reverse
      ? String.substring(str, ~start=pointer, ~end=start)
      : String.substring(str, ~start, ~end=pointer)
    switch substring {
    | "one" | "1" => Some("1")
    | "two" | "2" => Some("2")
    | "three" | "3" => Some("3")
    | "four" | "4" => Some("4")
    | "five" | "5" => Some("5")
    | "six" | "6" => Some("6")
    | "seven" | "7" => Some("7")
    | "eight" | "8" => Some("8")
    | "nine" | "9" => Some("9")
    | _ =>
      // reached end of string
      if start === end_of_loop && pointer === end_of_loop {
        None
      // pointer reached enf of string, increment starter and reset pointer
      } else if pointer == end_of_loop {
        aux(str, ~start=start + increment, ~pointer=reverse ? String.length(str) : 0, ~reverse)
      } else {
        aux(str, ~start, ~pointer=pointer + increment, ~reverse)
      }
    }
  }
  aux(
    str,
    // start pointer and start at the end of the string if reverse is true
    ~start=reverse ? String.length(str) : 0,
    ~pointer=reverse ? String.length(str) - 1 : 1,
    ~reverse,
  )
}

let res = Array.reduce(lines, 0, (acc, line) => {
  let first_digit = find_first(line, ~reverse=false)
  let last_digit = find_first(line, ~reverse=true)

  let first_and_last = Option.getOr(first_digit, "") ++ Option.getOr(last_digit, "")

  let num = Option.getOr(Int.fromString(first_and_last), 0)

  acc + num
})

Console.log(res)
