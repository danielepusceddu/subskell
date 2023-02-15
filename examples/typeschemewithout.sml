let id : 'a -> 'a = fun x -> x
in let eq: 'a -> 'a -> bool = (=)
in let five = id 5
in eq