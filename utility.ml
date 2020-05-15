let list_of count f=
  let modified_f _=f () (*f takes () as argument and not int as in List.init *)
  in List.init count modified_f
  ;;