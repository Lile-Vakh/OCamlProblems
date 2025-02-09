let rec count_leaves tree =
  match tree with
  |Empty -> 0
  |Node (_, Empty, Empty) -> 1
  |Node (_, l, r) -> count_leaves l + count_leaves r;;
    