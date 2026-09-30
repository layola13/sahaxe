class Arr {
	static function main():Void {
		var a:Array<Int> = [1, 2, 3, 4];
		var sum:Int = 0;
		var i:Int = 0;
		while (i < a.length) {
			sum = sum + a[i];
			i = i + 1;
		}
		a[0] = sum;
		trace("arr done");
	}
}
