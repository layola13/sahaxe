typedef Point = {
	var x:Int;
	var y:Int;
}

class Obj {
	static function main():Void {
		var p:Point = {x: 3, y: 4};
		var s:Int = p.x + p.y;
		p.x = 10;
		var t:Int = p.x * p.y;
		if (t > 20) {
			trace("big");
		} else {
			trace("small");
		}
	}
}
