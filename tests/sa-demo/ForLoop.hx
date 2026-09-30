class Counter {
	public var cur:Int;
	public var max:Int;

	public function new(max:Int) {
		this.cur = 0;
		this.max = max;
	}

	public function hasNext():Bool {
		return this.cur < this.max;
	}

	public function next():Int {
		var v:Int = this.cur;
		this.cur = this.cur + 1;
		return v;
	}
}

class ForLoop {
	static function main():Void {
		var sum:Int = 0;
		for (i in 0...10) {
			sum = sum + i;
		}
		var arr:Array<Int> = [5, 6, 7];
		var total:Int = 0;
		for (v in arr) {
			if (v == 6) {
				continue;
			}
			total = total + v;
		}
		var it = new Counter(5);
		var cs:Int = 0;
		while (it.hasNext()) {
			cs = cs + it.next();
		}
		if (sum == 45 && total == 12 && cs == 10) {
			trace("for ok");
		} else {
			trace("for bad");
		}
	}
}
