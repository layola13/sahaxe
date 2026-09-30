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

package sa.net;

/**
	Minimal UDP surface over `sci/sa_std` net contracts (v0.23).

	Handles are opaque `UInt` values like `Tcp`. Failures abort loudly
	via `panic`. `recv` returns the datagram bytes (`__retlen`
	convention shared with String returns).
**/
extern class Udp {
	/** Bind 127.0.0.1:port (0 = ephemeral), returns socket handle. */
	public static function bind(port:Int):UInt;

	/** Bound port of a socket created with port 0. */
	public static function port(socket:UInt):Int;

	/** Connect a socket to a peer (loopback to self works). */
	public static function connect(socket:UInt, host:String, port:Int):Void;

	/** Send bytes to the connected peer, returns bytes sent. */
	public static function send(socket:UInt, data:String):Int;

	/** Receive one datagram (up to maxBytes). */
	public static function recv(socket:UInt, maxBytes:Int):String;

	/** Set the read timeout in milliseconds. */
	public static function setReadTimeout(socket:UInt, ms:Int):Void;

	/** Close a socket. */
	public static function close(socket:UInt):Void;
}
