typedef Point = {
	var x:Int;
	var y:Int;
}

class ObjArr {
	static function main():Void {
		var pts:Array<Point> = [{x: 1, y: 2}, {x: 10, y: 20}];
		var i:Int = 0;
		var total:Int = 0;
		while (i < 2) {
			var q:Point = pts[i];
			total = total + q.x + q.y;
			i = i + 1;
		}
		var p:Point = {x: 5, y: 6};
		p.x = p.x + total;
		if (p.x == 38) {
			trace("objarr ok");
		} else {
			trace("objarr bad");
		}
	}
}
