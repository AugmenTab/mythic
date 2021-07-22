Handlebars.registerHelper("concat", function() {
  let str = "";
  for(var arg in arguments){
    if(typeof arguments[arg] != "object"){
      str += arguments[arg];
    }
  }
  return str;
});

Handlebars.registerHelper("localnum", function(num) {
  return num.toLocaleString();
});