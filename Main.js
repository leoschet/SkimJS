var l = [1,2,3];
var l1 = [1,3,4];
var l2 = [];
var l5 = [1,2];
var l3 = [1,2,3];
var hl = head(l);
var hl = tail(l);
var t = (l == l1);
var u = (l == l2);
var w = (l == l3);
var str = ["castorzinho"];
var str2 = ["castorzinho"];
var str3 = ["plc", "choramais"];
var comp = (str == str2);
var comp2 = (str == str3);
var z = 10;
/*
tail(y);
*/
//len(x);


function auxFunc(number1,  number2) {
	var k = 14;
	k = k + number1;	
	z = z + number2;
	return number1 + number2;
}

auxFunc(4, 1);

//hl;
function chirp(n) {
  if( n < 1 ){
  	return
  } else {
  	var e = n;
  	chirp(n-1);
  }
}
chirp(3);