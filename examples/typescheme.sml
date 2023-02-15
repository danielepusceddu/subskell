let id : 'a. 'a -> 'a = fun x -> x
in let eq: 'a. 'a -> 'a -> bool = (=)
in let five = id 5
in eq