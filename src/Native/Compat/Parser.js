/*

import Elm.Kernel.Utils exposing (chr, Tuple2, Tuple3)

*/



// STRINGS


var _proda_ai$elm_parser$Native_Compat_Parser = function() {
    function Tuple3(x, y, z)
    {
        return {
            ctor: '_Tuple3',
            _0: x,
            _1: y,
            _2: z
        };
    }

    var isSubString = F5(function(smallString, offset, row, col, bigString)
    {
        var smallLength = smallString.length;
        var isGood = offset + smallLength <= bigString.length;

        for (var i = 0; isGood && i < smallLength; )
        {
            var code = bigString.charCodeAt(offset);
            isGood =
                smallString[i++] === bigString[offset++]
                && (
                    code === 0x000A /* \n */
                        ? ( row++, col=1 )
                        : ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
                )
        }

        return Tuple3(isGood ? offset : -1, row, col);
    });


    // CHARS


    var isSubChar = F3(function(predicate, offset, string)
    {
        return (
            string.length <= offset
                ? -1
                :
            (string.charCodeAt(offset) & 0xF800) === 0xD800
                ? (predicate(_elm_lang$core$Native_Utils.chr(string.substr(offset, 2))) ? offset + 2 : -1)
                :
            (predicate(_elm_lang$core$Native_Utils.chr(string[offset]))
                ? ((string[offset] === '\n') ? -2 : (offset + 1))
                : -1
            )
        );
    });


    var isAsciiCode = F3(function(code, offset, string)
    {
        return string.charCodeAt(offset) === code;
    });



    // NUMBERS


    var chompBase10 = F2(function(offset, string)
    {
        for (; offset < string.length; offset++)
        {
            var code = string.charCodeAt(offset);
            if (code < 0x30 || 0x39 < code)
            {
                return offset;
            }
        }
        return offset;
    });


    var consumeBase = F3(function(base, offset, string)
    {
        for (var total = 0; offset < string.length; offset++)
        {
            var digit = string.charCodeAt(offset) - 0x30;
            if (digit < 0 || base <= digit) break;
            total = base * total + digit;
        }
        return _elm_lang$core$Native_Utils.Tuple2(offset, total);
    });


    var consumeBase16 = F2(function(offset, string)
    {
        for (var total = 0; offset < string.length; offset++)
        {
            var code = string.charCodeAt(offset);
            if (0x30 <= code && code <= 0x39)
            {
                total = 16 * total + code - 0x30;
            }
            else if (0x41 <= code && code <= 0x46)
            {
                total = 16 * total + code - 55;
            }
            else if (0x61 <= code && code <= 0x66)
            {
                total = 16 * total + code - 87;
            }
            else
            {
                break;
            }
        }
        return _elm_lang$core$Native_Utils.Tuple2(offset, total);
    });



    // FIND STRING


    var findSubString = F5(function(smallString, offset, row, col, bigString)
    {
        var newOffset = bigString.indexOf(smallString, offset);
        var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

        while (offset < target)
        {
            var code = bigString.charCodeAt(offset++);
            code === 0x000A /* \n */
                ? ( col=1, row++ )
                : ( col++, (code & 0xF800) === 0xD800 && offset++ )
        }

        return Tuple3(newOffset, row, col);
    });

    return {
        isSubString: isSubString,
        isSubChar: isSubChar,
        isAsciiCode: isAsciiCode,
        chompBase10: chompBase10,
        consumeBase: consumeBase,
        consumeBase16: consumeBase16,
        findSubString: findSubString
    }
}();
