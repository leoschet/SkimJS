var arr = [1,2,3];
var arr1 = [1,2,3];
var arr2 = [45,6,2];

function fat(num){
	if (num == 1){
 		return num;
  	}

 	return fat(num-1) * num;
}

if(arr == arr2){
	var fatorial = fat(5);
} else{
	var fatorial = fat(3);

}


