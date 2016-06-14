function conca (ls, xs){
var c = len(lista1);
	return ls.concat(xs);
}

function quicksort(l){
	if(l == []){
		return [];
	}else{
	var list = tail(l);
	var menor = [];
	var maior = [];
	var result = [];
	var arrayHead = head(l);
		while (list != []) {
			if(head(l) <= head(list)){
				menor = conca(menor, arrayHead);
			}else{
				maior = conca(arrayHead, maior);
			}
			list = tail(list);
		}
		var major = conca(arrayHead, quicksort(maior));
		result = conca(quicksort(menor), major);
		return result;
	}
}

var lista1 = [12,1,4,9,32,22,55,1,9,0,5]
var z = quicksort(lista1); 