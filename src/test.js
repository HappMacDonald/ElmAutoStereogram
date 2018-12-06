// ife breathe a life breathe a life breathe a life breathe ma life breath ma life
// a life breathe
// ma life breath

// leftLen 1 -> leftPairs[2] = ["a", "ma"]
// rightLen 6 -> rightPairs[5] = ["breath", "breathe"]
parallax = 15
i = 0
message = [ ["life", 63] ];
leftPairs = [[],["a", "ma"]];
rightPairs = [[],[],[],[],[],[],["breath", "breathe"]]
do {
  var leftLen  = 1;
  var rightLen = parallax - message[i][0].length - leftLen - 4;
} while(!(leftpairs[leftLen]!=null && rightpairs[rightLen]!=null));
var left     = leftpairs[leftLen][0];
var right    = rightpairs[rightLen][0];
var str1     = left[0] +" "+ message[i][0] +" "+ right[1] +" ";
var str2     = left[1] +" "+ message[i][0] +" "+ right[0] +" ";

console.log([leftLen - message[i][1]%parallax + parallax, str1, message[i][1] - leftLen, str2])


13 = leftLen - message[i][1]%parallax + parallax
62 = message[i][1] - leftLen