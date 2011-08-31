define(['thrift'], function (Thrift) {
    var exports = {}, TJSONProtocol;
    TJSONProtocol = exports.TJSONProtocol = function(transport) {
        this.transport = transport;
    };

    TJSONProtocol.Type = {};
    TJSONProtocol.Type[Thrift.Type.BOOL] = '"tf"';
    TJSONProtocol.Type[Thrift.Type.BYTE] = '"i8"';
    TJSONProtocol.Type[Thrift.Type.I16] = '"i16"';
    TJSONProtocol.Type[Thrift.Type.I32] = '"i32"';
    TJSONProtocol.Type[Thrift.Type.I64] = '"i64"';
    TJSONProtocol.Type[Thrift.Type.DOUBLE] = '"dbl"';
    TJSONProtocol.Type[Thrift.Type.STRUCT] = '"rec"';
    TJSONProtocol.Type[Thrift.Type.STRING] = '"str"';
    TJSONProtocol.Type[Thrift.Type.MAP] = '"map"';
    TJSONProtocol.Type[Thrift.Type.LIST] = '"lst"';
    TJSONProtocol.Type[Thrift.Type.SET] = '"set"';


    TJSONProtocol.RType = {};
    TJSONProtocol.RType.tf = Thrift.Type.BOOL;
    TJSONProtocol.RType.i8 = Thrift.Type.BYTE;
    TJSONProtocol.RType.i16 = Thrift.Type.I16;
    TJSONProtocol.RType.i32 = Thrift.Type.I32;
    TJSONProtocol.RType.i64 = Thrift.Type.I64;
    TJSONProtocol.RType.dbl = Thrift.Type.DOUBLE;
    TJSONProtocol.RType.rec = Thrift.Type.STRUCT;
    TJSONProtocol.RType.str = Thrift.Type.STRING;
    TJSONProtocol.RType.map = Thrift.Type.MAP;
    TJSONProtocol.RType.lst = Thrift.Type.LIST;
    TJSONProtocol.RType.set = Thrift.Type.SET;

    TJSONProtocol.Version = 1;

    TJSONProtocol.prototype = {

        flush: function() {
            return this.transport.flush();
        },

        getTransport: function() {
            return this.transport;
        },

        //Write functions
        writeMessageBegin: function(name, messageType, seqid) {
            this.tstack = [];
            this.tpos = [];

            this.tstack.push([TJSONProtocol.Version, '"' +
                name + '"', messageType, seqid]);
        },

        writeMessageEnd: function() {
            var obj = this.tstack.pop();

            this.wobj = this.tstack.pop();
            this.wobj.push(obj);

            this.wbuf = '[' + this.wobj.join(',') + ']';

            this.transport.write(this.wbuf);
         },


        writeStructBegin: function(name) {
            this.tpos.push(this.tstack.length);
            this.tstack.push({});
        },

        writeStructEnd: function() {

            var p = this.tpos.pop();
            var struct = this.tstack[p];
            var str = '{';
            var first = true;
            for (var key in struct) {
                if (first) {
                    first = false;
                } else {
                    str += ',';
                }

                str += key + ':' + struct[key];
            }

            str += '}';
            this.tstack[p] = str;
        },

        writeFieldBegin: function(name, fieldType, fieldId) {
            this.tpos.push(this.tstack.length);
            this.tstack.push({ 'fieldId': '"' +
                fieldId + '"', 'fieldType': TJSONProtocol.Type[fieldType]
            });

        },

        writeFieldEnd: function() {
            var value = this.tstack.pop();
            var fieldInfo = this.tstack.pop();

            this.tstack[this.tstack.length - 1][fieldInfo.fieldId] = '{' +
                fieldInfo.fieldType + ':' + value + '}';
            this.tpos.pop();
        },

        writeFieldStop: function() {
            //na
        },

        writeMapBegin: function(keyType, valType, size) {
            //size is invalid, we'll set it on end.
            this.tpos.push(this.tstack.length);
            this.tstack.push([TJSONProtocol.Type[keyType],
                TJSONProtocol.Type[valType], 0]);
        },

        writeMapEnd: function() {
            var p = this.tpos.pop();

            if (p == this.tstack.length) {
                return;
            }

            if ((this.tstack.length - p - 1) % 2 !== 0) {
                this.tstack.push('');
            }

            var size = (this.tstack.length - p - 1) / 2;

            this.tstack[p][this.tstack[p].length - 1] = size;

            var map = '}';
            var first = true;
            while (this.tstack.length > p + 1) {
                var v = this.tstack.pop();
                var k = this.tstack.pop();
                if (first) {
                    first = false;
                } else {
                    map = ',' + map;
                }

                map = '"' + k + '":' + v + map;
            }
            map = '{' + map;

            this.tstack[p].push(map);
            this.tstack[p] = '[' + this.tstack[p].join(',') + ']';
        },

        writeListBegin: function(elemType, size) {
            this.tpos.push(this.tstack.length);
            this.tstack.push([TJSONProtocol.Type[elemType], size]);
        },

        writeListEnd: function() {
            var p = this.tpos.pop();

            while (this.tstack.length > p + 1) {
                var tmpVal = this.tstack[p + 1];
                this.tstack.splice(p + 1, 1);
                this.tstack[p].push(tmpVal);
            }

            this.tstack[p] = '[' + this.tstack[p].join(',') + ']';
        },

        writeSetBegin: function(elemType, size) {
            this.tpos.push(this.tstack.length);
            this.tstack.push([TJSONProtocol.Type[elemType], size]);
        },

        writeSetEnd: function() {
            var p = this.tpos.pop();

            while (this.tstack.length > p + 1) {
                var tmpVal = this.tstack[p + 1];
                this.tstack.splice(p + 1, 1);
                this.tstack[p].push(tmpVal);
            }

            this.tstack[p] = '[' + this.tstack[p].join(',') + ']';
        },

        writeBool: function(value) {
            this.tstack.push(value ? 1 : 0);
        },

        writeByte: function(i8) {
            this.tstack.push(i8);
        },

        writeI16: function(i16) {
            this.tstack.push(i16);
        },

        writeI32: function(i32) {
            this.tstack.push(i32);
        },

        writeI64: function(i64) {
            this.tstack.push(i64);
        },

        writeDouble: function(dbl) {
            this.tstack.push(dbl);
        },

        writeString: function(str) {
            // We do not encode uri components for wire transfer:
            if (str === null) {
                this.tstack.push(null);
            } else {
                // concat may be slower than building a byte buffer
                var escapedString = '';
                for (var i = 0; i < str.length; i++) {
                    var ch = str.charAt(i);      // a single double quote: "
                    if (ch === '\"') {
                        escapedString += '\\\"'; // write out as: \"
                    } else if (ch === '\\') {    // a single backslash: \
                        escapedString += '\\\\'; // write out as: \\
                    /* Currently escaped forward slashes break TJSONProtocol.
                     * As it stands, we can simply pass forward slashes into
                     * our strings across the wire without being escaped.
                     * I think this is the protocol's bug, not thrift.js
                     * } else if(ch === '/') {   // a single forward slash: /
                     *  escapedString += '\\/';  // write out as \/
                     * }
                     */
                    } else if (ch === '\b') {    // a single backspace: invisible
                        escapedString += '\\b';  // write out as: \b"
                    } else if (ch === '\f') {    // a single formfeed: invisible
                        escapedString += '\\f';  // write out as: \f"
                    } else if (ch === '\n') {    // a single newline: invisible
                        escapedString += '\\n';  // write out as: \n"
                    } else if (ch === '\r') {    // a single return: invisible
                        escapedString += '\\r';  // write out as: \r"
                    } else if (ch === '\t') {    // a single tab: invisible
                        escapedString += '\\t';  // write out as: \t"
                    } else {
                        escapedString += ch;     // Else it need not be escaped
                    }
                }
                this.tstack.push('"' + escapedString + '"');
            }
        },

        writeBinary: function(str) {
            this.writeString(str);
        },



        // Reading functions
        readMessageBegin: function(name, messageType, seqid) {
            this.rstack = [];
            this.rpos = [];

            if (typeof jQuery !== 'undefined') {
                this.robj = jQuery.parseJSON(this.transport.readAll());
            } else {
                this.robj = eval(this.transport.readAll());
            }

            var r = {};
            var version = this.robj.shift();

            if (version != TJSONProtocol.Version) {
                throw 'Wrong thrift protocol version: ' + version;
            }

            r.fname = this.robj.shift();
            r.mtype = this.robj.shift();
            r.rseqid = this.robj.shift();


            //get to the main obj
            this.rstack.push(this.robj.shift());

            return r;
        },

        readMessageEnd: function() {
        },

        readStructBegin: function(name) {
            var r = {};
            r.fname = '';

            //incase this is an array of structs
            if (this.rstack[this.rstack.length - 1] instanceof Array) {
                this.rstack.push(this.rstack[this.rstack.length - 1].shift());
            }

            return r;
        },

        readStructEnd: function() {
            if (this.rstack[this.rstack.length - 2] instanceof Array) {
                this.rstack.pop();
            }
        },

        readFieldBegin: function() {
            var r = {};

            var fid = -1;
            var ftype = Thrift.Type.STOP;

            //get a fieldId
            for (var f in (this.rstack[this.rstack.length - 1])) {
                if (f === null) {
                  continue;
                }

                fid = parseInt(f, 10);
                this.rpos.push(this.rstack.length);

                var field = this.rstack[this.rstack.length - 1][fid];

                //remove so we don't see it again
                delete this.rstack[this.rstack.length - 1][fid];

                this.rstack.push(field);

                break;
            }

            if (fid != -1) {

                //should only be 1 of these but this is the only
                //way to match a key
                for (var i in (this.rstack[this.rstack.length - 1])) {
                    if (TJSONProtocol.RType[i] === null) {
                        continue;
                    }

                    ftype = TJSONProtocol.RType[i];
                    this.rstack[this.rstack.length - 1] =
                        this.rstack[this.rstack.length - 1][i];
                }
            }

            r.fname = '';
            r.ftype = ftype;
            r.fid = fid;

            return r;
        },

        readFieldEnd: function() {
            var pos = this.rpos.pop();

            //get back to the right place in the stack
            while (this.rstack.length > pos) {
                this.rstack.pop();
            }

        },

        readMapBegin: function(keyType, valType, size) {
            var map = this.rstack.pop();

            var r = {};
            r.ktype = TJSONProtocol.RType[map.shift()];
            r.vtype = TJSONProtocol.RType[map.shift()];
            r.size = map.shift();


            this.rpos.push(this.rstack.length);
            this.rstack.push(map.shift());

            return r;
        },

        readMapEnd: function() {
            this.readFieldEnd();
        },

        readListBegin: function(elemType, size) {
            var list = this.rstack[this.rstack.length - 1];

            var r = {};
            r.etype = TJSONProtocol.RType[list.shift()];
            r.size = list.shift();

            this.rpos.push(this.rstack.length);
            this.rstack.push(list);

            return r;
        },

        readListEnd: function() {
            this.readFieldEnd();
        },

        readSetBegin: function(elemType, size) {
            return this.readListBegin(elemType, size);
        },

        readSetEnd: function() {
            return this.readListEnd();
        },

        readBool: function() {
            var r = this.readI32();

            if (r !== null && r.value == '1') {
                r.value = true;
            } else {
                r.value = false;
            }

            return r;
        },

        readByte: function() {
            return this.readI32();
        },

        readI16: function() {
            return this.readI32();
        },

        readI32: function(f) {
            if (f === undefined) {
                f = this.rstack[this.rstack.length - 1];
            }

            var r = {};

            if (f instanceof Array) {
                if (f.length === 0) {
                    r.value = undefined;
                } else {
                    r.value = f.shift();
                }
            } else if (f instanceof Object) {
               for (var i in f) {
                    if (i === null) {
                      continue;
                    }
                    this.rstack.push(f[i]);
                    delete f[i];

                    r.value = i;
                    break;
               }
            } else {
                r.value = f;
                this.rstack.pop();
            }

            return r;
        },

        readI64: function() {
            return this.readI32();
        },

        readDouble: function() {
            return this.readI32();
        },

        readString: function() {
            var r = this.readI32();
            return r;
        },

        readBinary: function() {
            return this.readString();
        },


        //Method to arbitrarily skip over data.
        skip: function(type) {
            throw 'skip not supported yet';
        }
    };
    return exports;
});
 
