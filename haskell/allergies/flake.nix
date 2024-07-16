{
  description = "Dominoes flake";

  inputs = { 
    dev.url = "../";
  };

  outputs = { self, dev } :  
  let 
    name = builtins.baseNameOf (builtins.toString ./.);
  in 

  {
    devShell = dev.devShell name; 
  };
}
