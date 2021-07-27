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

Handlebars.registerHelper("times", function(n, block) {
  let accum = "";
  for (let i = 0; i < n; i++) { accum += block.fn(i); }
  return accum;
});