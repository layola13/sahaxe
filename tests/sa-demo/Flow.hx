class Flow {
	static function main():Void {
		var n:Int = 10;
		var acc:Int = 0;
		var i:Int = 0;
		while (i < n) {
			i = i + 1;
			if (i == 5) {
				continue;
			}
			if (i > 7) {
				break;
			}
			acc = acc + i;
		}
		if (acc > 0) {
			trace("pos");
		} else {
			trace("neg");
		}
	}
}
