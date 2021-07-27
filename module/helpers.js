Handlebars.registerHelper("concat", function() {
  let str = "";
  for(var arg in arguments){
    if(typeof arguments[arg] != "object"){
      str += arguments[arg];
    }
  }
  return str;
});

Handlebars.registerHelper('cond', function(operator, v1, v2) {
  switch (operator) {
    case '==': return (v1 == v2);
    case '===': return (v1 === v2);
    case '!=': return (v1 != v2);
    case '!==': return (v1 !== v2);
    case '<': return (v1 < v2);
    case '<=': return (v1 <= v2);
    case '>': return (v1 > v2);
    case '>=': return (v1 >= v2);
    case '&&': return (v1 && v2);
    case '||': return (v1 || v2);
    default: return options.inverse(this);
  }
});

Handlebars.registerHelper("localnum", function(num) {
  return num.toLocaleString();
});

Handlebars.registerHelper("not", function(arg) {
  return !arg;
});

Handlebars.registerHelper("times", function(n, block) {
  let accum = "";
  for (let i = 0; i < n; i++) { accum += block.fn(i); }
  return accum;
});