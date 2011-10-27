var svgNS = "http://www.w3.org/2000/svg";

function assert(expr) {
    if (! expr) {
	alert("assert failed: expr = " + expr);
    }
}

function build_tree(words) {
    var tree = {};
    var i;
    for (i = 0; i < words.length; i++) {
	var w = words[i];
	var node = tree;
	var j;
	for (j = 0; j < w.length; j++) {
	    var nn = node[w[j]];
	    if (! nn) {
		nn = {};
		node[w[j]] = nn;
	    }
	    node = nn;
	}
	node.isterminal = true;
    }
    return tree;
}

function build_regexp(tree) {
    var k;
    var values = [];
    var ret;
    for (k in tree) {
	if ((tree.hasOwnProperty(k)) && (k != "isterminal")) {
	    var v = build_regexp(tree[k]);
	    var kk = k.match("[a-zA-Z0-9]") ? k : "\\" + k;
	    if (v) {
		//		vv = "(?:" + kk + v + ")";
		values.push(kk + v);
	    }
	    if (!v || tree[k].isterminal) {
		values.push(kk);
	    }
	}
    }
    if (values.length === 0) {
	return null;
    }
    if (values.length == 1) {
	return values[0] + (tree.isterminal ? '?' : '');
    }
    else {
	ret = "(?:";
	var i;
	for (i = 0; i < values.length; i++) {
	    if (i > 0) {
		ret += "|";
	    }
	    ret += values[i];
	}
	ret += ")";
	if (tree.isterminal) {
	    ret += '?';
	}
    }
    return ret;
}

function regexp_optimize(words) {
    var tree = build_tree(words);
    var re_str = build_regexp(tree);
    return re_str;
}

function hp41barcodegenerator(properties) {
    this.xspacing = 1.75;
    this.zerowidth = this.xspacing * 0.9;
    this.onewidth = this.xspacing * 1.9;
    this.yspacing = 12;
    this.fontsize = 10;
    this.height = 30;
    this.xoffset = 20;
    this.yoffset = 20;
    this.x = this.xoffset;
    this.y = this.yoffset;
    this.rownum = 1;
    this.rowspersheet = 16;
    var k;
    for (k in properties) { 
	if (properties.hasOwnProperty(k)) {
	    this[k] = properties[k];
	}
    }
    this.checksum = 0;
    this.leadingbytes = 0;
    this.trailingbytes = 0;
    this.sequencenumber = 0;
    if (this.parentelementid) {
	this.parentelement = document.getElementById(this.parentelementid);
    }
    else {
	this.parentelement = document.getElementsByTagName("BODY")[0];
    }
}

hp41barcodegenerator.prototype.newsheet = function () {
    this.svgelement = document.createElementNS(svgNS, "svg");
    this.svgelement.setAttribute("style", "font-size: " + this.fontsize);
    this.parentelement.appendChild(this.svgelement);
    this.x = this.xoffset;
    this.y = this.yoffset;
}

hp41barcodegenerator.prototype.emitbit = function(bit) {
    var width = bit === 1 ? this.onewidth : this.zerowidth;
    var rect = document.createElementNS(svgNS, "rect");
    rect.setAttribute("x", this.x);
    rect.setAttribute("y", this.y);
    rect.setAttribute("width", width);
    rect.setAttribute("height", this.height);
    rect.setAttribute("fill", "black");
    this.svgelement.appendChild(rect);
    this.x += (width + this.xspacing);
}

hp41barcodegenerator.prototype.emitbyte = function(byte) {
    var mask = 128;
    while (mask !== 0) {
	var bit = (byte & mask) === 0 ? 0 : 1;
	this.emitbit(bit);
	mask >>= 1;
    }
}

hp41barcodegenerator.prototype.emitbytes = function(bytes) {
    var i;
    for (i = 0; i < bytes.length; i++) {
	this.emitbyte(bytes[i]);
    }
}

hp41barcodegenerator.prototype.header = function () {
    this.emitbit(0);
    this.emitbit(0);
}

hp41barcodegenerator.prototype.trailer = function () {
    this.emitbit(1);
    this.emitbit(0);
}

hp41barcodegenerator.prototype.newrow = function () {
    this.x = this.xoffset;
    this.y += (this.height);
    this.rownum += 1;
}

hp41barcodegenerator.prototype.row = function (bytes) {
    if (((this.rownum - 1) % this.rowspersheet) === 0) {
	this.newsheet();
    }
    var text = document.createElementNS(svgNS, "text");
    text.setAttribute("x", this.xoffset);
    text.setAttribute("y", this.y + this.fontsize);
    text.setAttribute("fill", "black");
    text.appendChild(document.createTextNode("ROW " + this.rownum))
    this.svgelement.appendChild(text);
    this.y += this.yspacing;
    this.header();
    this.emitbytes(bytes);
    this.trailer();
    this.newrow();
}


function hp41codebatcher () {
    this.generator = new hp41barcodegenerator({parentelementid: "barcodes"});
    this.typeindicator = 1; // set to 2 for "private"
    this.bytes = [];
    this.checksum = 0;
    this.seqno = 0;
    this.remainder = 0;
}

hp41codebatcher.prototype.emit = function(items) {
    var len1 = this.bytes.length;
    var i;
    for (i = 0; i < items.length; i++) {
	var item = items[i];
	if (typeof(item) === "number") {
	    this.bytes.push(item);
	}
	else {
	    var j;
	    for (j = 0; j < item.length; j++) {
		this.bytes.push(item[j]);
	    }
	}
    }
    var len2 = this.bytes.length;
    if (len2 >= 13) {
	this.batchout(len1, len2);
    }
}

function max(a, b) { return a > b ? a : b; }
function min(a, b) { return a < b ? a : b; }

hp41codebatcher.prototype.batchout = function (len1, len2) {
    var buffer = [];
    var i;
    var numbytes = min(13, this.bytes.length);
    for (i = 0; i < numbytes; i++) {
	buffer.push(this.bytes.shift());
    }
    var partial = len2 > 13 ? 13 - len1 : 0;
    buffer.unshift((this.remainder << 4) | partial);
    buffer.unshift((this.typeindicator << 4) | this.seqno);
    buffer.unshift(this.checksum);
    var checksum = 0;
    for (i = 0; i < buffer.length; i++) {
	checksum += buffer[i];
    }
    checksum = (checksum & 0xff) + (checksum >> 8);
    checksum = (checksum & 0xff) + (checksum >> 8);
    this.checksum = checksum;
    buffer[0] = checksum;
    this.generator.row(buffer);
    this.remainder = this.bytes.length;
    this.seqno++;
    this.seqno &= 0x0f;
}

hp41codebatcher.prototype.finish = function () {
    if (this.bytes.length > 0) {
	this.batchout(this.bytes.length, this.bytes.length);
    }
}


function hp41codeparser (program) {
    this.sourcecode = program;
    this.hp41codebatcher = new hp41codebatcher();
    this.pos = 0;
}

hp41codeparser.prototype.shortlabels = {
    "A": 102, "B": 102, "C": 103, "D": 104, "E": 105,
    "F": 106, "G": 107, "H": 108, "I": 109, "J": 110,
    "a": 123, "b": 124, "c": 125, "d": 126, "e": 127
}

hp41codeparser.prototype.stackitems = {
    "X": 115, "Y": 114, "Z": 113, "T": 112, "L": 116
}

hp41codeparser.prototype.decode_numeric_arg = function(str) {
    return this.stackitems[str] || parseInt(str, 10);
}

hp41codeparser.prototype.skip = function(charstoskip) {
    this.sourcecode = this.sourcecode.substring(charstoskip);
    this.pos += charstoskip;
}

hp41codeparser.prototype.skip_sep = function () {
    var m; 
    m = this.sourcecode.match("^(?:\\s+|(?:\\s*;\\s*))");
    if (m) {
	this.skip(m[0].length);
    }
}

hp41codeparser.prototype.translatechars = function(text) {
    var ret = [];
    var i;
    for (i = 0; i < text.length; i++) {
	var c = text.charAt(i);
	if (c === "&") {
	    ret.push(126);
	}
	else if (c === "@") {
	    ret.push(13);
	}
	else if (c === "#") {
	    ret.push(29);
	}
	else {
	    ret.push(text.charCodeAt(i));
	}
    }
    return ret;
}
hp41codeparser.prototype.match_lbl = function() {
    if (! this.sourcecode.match("^LBL\\s")) {
	return false;
    }
    var m;
    m = this.sourcecode.match("^LBL\\s+(\\d\\d)");
    if (m) {
	this.skip(m[0].length);
	var lbl = parseInt(m[1], 10);
	if (lbl < 15) {
	    this.hp41codebatcher.emit([lbl + 1]);
	}
	else {
	    this.hp41codebatcher.emit([0xcf, lbl]);
	}
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^LBL\\s+([a-eA-J])");
    if (m) {
	this.skip(m[0].length);
	this.hp41codebatcher.emit([0xcf, this.shortlabels[m[1]]]);
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^LBL\\s\"([^\"]{1,15})\"");
    if (m) {
	this.skip(m[0].length);
	bytes = this.translatechars(m[1]);
	this.hp41codebatcher.emit([0xc0, 0x00, 0xf0 + bytes.length + 1, 
				   0x00, bytes]);
	return true;
    }
    // ELSE
    return false;
}

hp41codeparser.prototype.match_xeq = function () {
    if (! this.sourcecode.match("^XEQ\\s")) {
	return false;
    }
    // ELSE
    var m;
    m = this.sourcecode.match("^XEQ\\s+IND\\s+(\\d\\d)");
    if (m) {
	this.skip(m[0].length);
	var lbl = parseInt(m[1], 10);
	this.hp41codebatcher.emit([0xae, lbl | 0x80]);
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^XEQ\\s+IND\\s+([XYZTL])");
    if (m) {
	this.skip(m[0].length);
	var lbl = this.stackitems[m[1]];
	this.hp41codebatcher.emit([0xae, lbl | 0x80]);
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^XEQ\\s+(\\d\\d)");
    if (m) {
	this.skip(m[0].length);
	var lbl = parseInt(m[1], 10);
	this.hp41codebatcher.emit([0xe0, 0x00, lbl]);
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^XEQ\\s+([a-eA-J])");
    if (m) {
	this.skip(m[0].length);
	this.hp41codebatcher.emit([0xe0, 0x00, this.shortlabels[m[1]]]);
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^XEQ\\s\"([^\"]{1,15})\"");
    if (m) {
	this.skip(m[0].length);
	bytes = this.translatechars(m[1]);
	this.hp41codebatcher.emit([0x1e, 0xf0 + bytes.length, bytes]);
	return true;
    }
    // ELSE
    return false;
}

hp41codeparser.prototype.match_gto = function () {
    if (! this.sourcecode.match("^GTO\\s")) {
	return false;
    }
    // ELSE
    var m;
    m = this.sourcecode.match("^GTO\\s+IND\\s+(\\d\\d)");
    if (m) {
	this.skip(m[0].length);
	var lbl = parseInt(m[1], 10);
	this.hp41codebatcher.emit([0xae, lbl]);
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^GTO\\s+IND\\s+([XYZTL])");
    if (m) {
	this.skip(m[0].length);
	var lbl = this.stackitems[m[1]];
	this.hp41codebatcher.emit([0xae, lbl]);
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^GTO\\s+(\\d\\d)");
    if (m) {
	this.skip(m[0].length);
	var lbl = parseInt(m[1], 10);
	if (lbl < 15) {
	    this.hp41codebatcher.emit([0xb1 + lbl, 0x00]);
	}
	else {
	    this.hp41codebatcher.emit([0xd0, 0x00, lbl]);
	}
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^GTO\\s+([a-eA-J])");
    if (m) {
	this.skip(m[0].length);
	this.hp41codebatcher.emit([0xd0, this.shortlabels[m[1]]]);
	return true;
    }
    // ELSE
    m = this.sourcecode.match("^GTO\\s\"([^\"]{1,15})\"");
    if (m) {
	this.skip(m[0].length);
	bytes = this.translatechars(m[1]);
	this.hp41codebatcher.emit([0x1d, 0xf0 + bytes.length, bytes]);
	return true;
    }
    // ELSE
    return false;
}

hp41codeparser.prototype.match_reg = function () {
    var m;
    m =	this.sourcecode.match("^(RCL|STO)\\s+((?:IND\\s+)?)(\\d{2}|[XYZTL])");
    if (m) {
	this.skip(m[0].length);
	var reg = (m[3].length === 2 ? parseInt(m[3], 10) :
		   this.stackitems[m[3]]) |
	    (m[2].length !== 0 ? 0x80 : 0x00);
	if (reg <= 15) {
	    this.hp41codebatcher.emit([(m[1] === "STO" ? 0x30 :
					0x20) | reg]);
	}
	else {
	    this.hp41codebatcher.emit([(m[1] === "STO" ? 0x91 : 0x90),
				       reg]);
	}
	return true;
    }
    // ELSE
    m =	this.sourcecode.match("^(ARCL|ASTO)\\s+((?:IND\\s+)?)(\\d{2}|[XYZTL])");
    if (m) {
	this.skip(m[0].length);
	var reg = (m[3].length === 2 ? parseInt(m[3], 10) :
		   this.stackitems[m[3]]) |
	    (m[2].length !== 0 ? 0x80 : 0x00);
	this.hp41codebatcher.emit([(m[1] === "ASTO" ? 0x9a : 0x9b),
				   reg]);
	return true;
    }
    // ELSE
    return false;
}

hp41codeparser.prototype.re_alpha = '^((?:-?\\>)?)"([^"]*)"';
hp41codeparser.prototype.match_alpha = function () {
    var m = this.sourcecode.match(this.re_alpha);
    if (m) {
	var append = m[1] ? true : false;
	assert(m[2].length <= (append ? 14 : 15));
	this.skip(m[0].length);
	var i;
	bytes = [0xf0 + m[2].length + (append ? 1 : 0)];
	if (append) {
	    bytes.push(0x7f);
	}
	for (i = 0; i < m[2].length; i++) {
	    bytes.push(m[2].charCodeAt(i));
	}
	this.hp41codebatcher.emit(bytes);
	return true;
    }
    return false;
}

hp41codeparser.prototype.re_number = new
RegExp("^[-+]?(?:(?:\\d+(?:\\.\\d*)?)|(?:\\.\\d+))(?:[eE][-+]?\\d{1,2})?\\b");

hp41codeparser.prototype.match_number = function () {
    var m;
    m = this.sourcecode.match(this.re_number);
    if (! m) {
	return false;
    }
    // ELSE
    var first = true;
    while (m) {
	var i;
	var bytes = [];
	if (! first) {
	    this.hp41codebatcher.emit([0]);
	}
	for (i = 0; i < m[0].length; i++) {
	    var c = m[0].charAt(i);
	    if (c === ".") {
		bytes.push(26);
	    }
	    else if (c === "+") {
		// ignore 
	    }
	    else if (c === "-") {
		bytes.push(28);
	    }
	    else if (c === "e" || c === "E") {
		bytes.push(27);
	    }
	    else {
		bytes.push(m[0].charCodeAt(i) - 48 + 16);
	    }
	}
	this.hp41codebatcher.emit(bytes);
	this.skip(m[0].length);
	this.skip_sep();
	m = this.sourcecode.match(this.re_number);
	first = false;
    }
    return true;
}

function gen_dict_re(dict) {
    var k;
    var list = [];
    for (k in dict) {
	if (dict.hasOwnProperty(k)) {
	    list.push(k);
	}
    }
    return regexp_optimize(list);
}

hp41codeparser.prototype.re_one_byte_builtins = 
    "^" + gen_dict_re(one_byte_builtins);
hp41codeparser.prototype.re_two_byte_builtins = 
    "^(" + gen_dict_re(two_byte_builtins) + ")\\s+(?:(IND)\\s+)?(\\d{1,2}|[XYZTL])\\b";

hp41codeparser.prototype.match_builtins = function () {
    var m;
    m = this.sourcecode.match(this.re_one_byte_builtins);
    if (m) {
	this.skip(m[0].length);
	var code = one_byte_builtins[m[0]];
	this.hp41codebatcher.emit([code]);
	return true;
    }
    // ELSE
    m = this.sourcecode.match(this.re_two_byte_builtins);
    if (m) {
	this.skip(m[0].length);
	var code = two_byte_builtins[m[1]];
	var data = this.decode_numeric_arg(m[3]);
	if (m[2]) {
	    data |= 0x80;
	}
	this.hp41codebatcher.emit([code, data]);
	return true;
    }
    
    return false;
}

var externals = {};

hp41codeparser.prototype.match_external = function () {
    var m;
    m = this.sourcecode.match(this.re_externals);
    if (m) {
	this.skip(m[0].length);
	var codes = externals[m[0]];
	var romid = codes[0];
	var functionid = codes[1];
	var id = 64 * romid + functionid;
	var b1 = (id >> 8) + 0xa0;
	var b2 = id & 0xff;
	assert((b1 >= 0xa0) && (b1 <= 0xa8));
	this.hp41codebatcher.emit([b1, b2]);
	return true;
    }
    return false;
}

hp41codeparser.prototype.match_xrom = function () {
    var m;
    m = this.sourcecode.match("^XROM (\\d+),\\s*(\\d+)")
    if (m) {
	var romid = parseInt(m[1], 10);
	var functionid = parseInt(m[2], 10);
	var id = 64 * romid + functionid;
	var b1 = (id >> 8) + 0xa0;
	var b2 = id & 0xff;
	assert((b1 >= 0xa0) && (b1 <= 0xa8));
	this.hp41codebatcher.emit([b1, b2]);
	return true;
    }
    return false;
}

hp41codeparser.prototype.parse = function () {
    init_externals();
    $("#barcodes").empty();
    // strip away line numbers
    this.sourcecode = this.sourcecode.replace(/^\d+ +/gm, "");

    this.skip_sep();
    while (this.sourcecode !== "") {
	var matched = 
	    this.match_lbl() ||
	    this.match_xeq() ||
	    this.match_gto() ||
	    this.match_reg() ||
	    this.match_alpha() ||
	    this.match_number() ||
	    this.match_builtins() ||
	    this.match_external() ||
	    this.match_xrom();
	if (! matched) {
	    matched = this.sourcecode.match("^END\\b");
	    if (matched) {
		this.skip(matched[0].length);
	    }
	    else {
		alert("ERROR at pos " + this.pos + "; '" 
		      + (this.sourcecode.length > 25 ?
			 this.sourcecode.substring(0, 21) + " ..." :
			 this.sourcecode)
		      + "' not matched."); 
		throw("die!");
	    }
	}
	// ELSE
	this.skip_sep();
    }
    this.hp41codebatcher.emit([192, 0, 47]);
    this.hp41codebatcher.finish();
}

function doit() {
    var program = document.getElementById("program").value;
    var parser = new hp41codeparser(program);
    parser.parse();
    $("#tabs").tabs("select", $("#tabs").tabs("length") - 1);
}

var externals;
var defaultmodules = {"CXextfcn": 1, "CXtime": 1, "advantage": 1,
		      "cardr": 1, "wand": 1, "yfns": 1};

function init_config () {
    var config = $("#config");
    $(config).html("<div style='float:left;'/><div style='float:right;'/></div style='clear: both;'/>");
    var table = $("<table/>").appendTo(config.children("div:first-child"));
    $(table).append("<tr><th colwidth'=2'>Modules</th></tr>");
    var k;
    for (k in xroms) {
	if (xroms.hasOwnProperty(k)) {
	    var item = $("<tr><td>" + k + "</td><td><input type='checkbox'/></td></tr>").appendTo(table);
	    $(item).find("input").attr("id", "chkbox_" + k);
	    if (defaultmodules[k] === 1) {
		$(item).find("input").attr("checked", "checked");
	    }
	}
    }
    $(config).children("div:nth-child(2)").append("<a href='#' onclick='doit(); return false;'>Do it!</a>");
}


function init_externals(modules) {
    externals = {};
    var k; 
    var conflicts = null;
    for (k in xroms) {
	if (xroms.hasOwnProperty(k) &&
	    $("#chkbox_" + k).prop("checked")) {
	    var module = xroms[k];
	    var kk;
	    for (kk in module) {
		if (module.hasOwnProperty(kk)) {
		    if (externals.hasOwnProperty(kk)) {
			conflicts = (conflicts == null ? kk : conflicts + ', ' + kk);
		    }
		    else {
			externals[kk] = module[kk]; 
		    }
		}
	    }
	}
    }
    if (conflicts != null) {
	alert("Warning: module conflicts for symbols: " + conflicts);
    }
    hp41codeparser.prototype.re_externals = "^" + gen_dict_re(externals);
}


function hp41barcode_init() {
    init_config();
    $("#tabs").tabs();
    $("a").button();
    $("textarea").resizable()
}