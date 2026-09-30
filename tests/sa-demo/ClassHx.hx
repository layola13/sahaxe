class Point {
	public var x:Int;
	public var y:Int;

	public function new(x:Int, y:Int) {
		this.x = x;
		this.y = y;
	}

	public function move(dx:Int, dy:Int):Void {
		this.x = this.x + dx;
		this.y = this.y + dy;
	}

	public function sum():Int {
		return this.x + this.y;
	}
}

class ClassHx {
	static function main():Void {
		var p = new Point(3, 4);
		p.move(10, 20);
		var s:Int = p.sum();
		if (s == 37) {
			trace("class ok");
		} else {
			trace("class bad");
		}
	}
}
