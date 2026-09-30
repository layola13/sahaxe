/*
 * Copyright (C)2005-2026 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package sa;

/**
	SA target runtime boot helpers (v0.1 stub).

	Full runtime (trace/print, string, array helpers lowered to
	`sci/sa_std` contracts) is pending, see `SA_TARGET.md`.
**/
@:dox(hide)
class Boot {
	/**
		Print a string to stdout. Lowered to `@sa_print_bytes`
		(`sa_std/io/print.sai`) by the SA generator.
	**/
	public static function trace(v:Dynamic):Void {}

	// --- v0.1 stubs, lowered to sci/sa_std contracts (see SA_TARGET.md) ---
	public static function __instanceof(v:Dynamic, t:Dynamic):Bool {
		return false;
	}

	public static function clampInt32(x:Float):Int {
		return 0;
	}

	public static function stringify(v:Dynamic):String {
		return "";
	}

	public static function parseIntPrefix(x:String):Null<Int> {
		return null;
	}

	public static function parseFloatPrefix(x:String):Float {
		return Math.NaN;
	}
}
