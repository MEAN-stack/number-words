// valid words
// we deal with 'zero', 'minus', and 'billion' as special cases and remove them from the input string before we validate the remaining words
// 
var wordValues = {
  'one': 1,
  'two': 2,
  'three': 3,
  'four': 4,
  'five': 5,
  'six': 6,
  'seven': 7,
  'eight': 8,
  'nine': 9,
  'ten': 10,
  'eleven': 11,
  'twelve': 12,
  'thirteen': 13,
  'fourteen': 14,
  'fifteen': 15,
  'sixteen': 16,
  'seventeen': 17,
  'eighteen': 18,
  'nineteen': 19,
  'twenty': 20,
  'thirty': 30,
  'forty': 40,
  'fifty': 50,
  'sixty': 60,
  'seventy': 70,
  'eighty': 80,
  'ninety': 90,
  'hundred': 100,
  'thousand': 1000,
  'million': 1000000
}

// check that all our words exist as keys in the wordValues object
function validate(words) {
  for (var i=0; i<words.length; i++) {
    if (!wordValues.hasOwnProperty(words[i])) {
      return false;
    }
  }
  return true;
}

function getUnits(words) {
  var word = words.pop();
  if (word === undefined) {
    throw 'no more words';
  }
  var value = wordValues[word];

  if ( value < 20 ) {
    return (value)
  }
  else {
    words.push(word)
    return 0;
  }
}

function getTens(words) {
  var word = words.pop();
  if (word === undefined) {
    throw 'no more words';
  }
  var value = wordValues[word];

  if ( value >= 20 && value < 100 ) {
    return (value)
  }
  else {
    words.push(word)
    return 0;
  }
}

function getHundreds(words) {
  var word = words.pop();
  if (word === undefined) {
    throw 'no more words';
  }
  if (word !== 'hundred') {
    words.push(word)
    return 0;
  }
  // find out how many hundreds we have
  var num = getUnits(words)
  if (num < 10) {
    return num * 100;
  }
  else {
    return Infinity;
  }
}

function getThousands(words) {
  var word = words.pop();
  if (word === undefined) {
    throw 'no more words';
  }
  if (word !== 'thousand') {
    words.push(word)
    return 0;
  }
  // find out how many thousands we have
  var num = getHtu(words)
  if ( num === 0) {
    return Infinity;
  }
  if (num < 1000) {
    return num * 1000;
  }
  else {
    return Infinity;
  }
}

function getMillions(words) {
  var word = words.pop();
  if (word === undefined) {
    throw 'no more words';
  }
  if (word !== 'million') {
    words.push(word)
    return 0;
  }
  // find out how many millions we have
  var num = getHtu(words)
  if ( num === 0) {
    return Infinity;
  }
  if (num < 1000) {
    return num * 1000000;
  }
  else {
    return Infinity;
  }
}

// get hundreds, tens, and units
function getHtu(words) {
  var result = 0;
  try {
    result = getUnits(words);
    if (result < 10) {
      result += getTens(words);
    }
    result += getHundreds(words);
    return result;
  }
  catch (ex) {
    return result;
  }
}

function evaluate(input) {
  // special cases
  if (input=='zero') {
    return 0;
  }
  if (input == 'one billion') {
    return 1000000000;
  }
  if (input == 'minus one billion') {
    return -1000000000;
  }

  // simplify whitespace
  var inputString = input.trim();
  inputString = inputString.replace(/\s\s+/g, ' ');
  inputString = inputString.replace(/ and /g, ' ');
//  console.log(inputString);
  var words = inputString.split(' ');

  // deal with 'minus'
  var sign = 1;
  if (words[0]=='minus') {
    sign = -1;
    words.splice(0, 1);
  }
  if (!validate(words)) {
    return Infinity;
  }

  // parse the list of words, from right to left
  var result = 0;

  try {
    result = getHtu(words);
    result += getThousands(words);
    result += getMillions(words);
    
    // we should have consumed all the words by now
    if (words.length === 0) {
      return result*sign;
    }
    else {
      return Infinity
    }
  }
  catch (ex) {
    return result*sign;
  }
}

// values less than 20 correspond to a single word
// but we don't normally print 'zero' 
var unit = [
  '',
  'one ',
  'two ',
  'three ',
  'four ',
  'five ',
  'six ',
  'seven ',
  'eight ',
  'nine ',
  'ten ',
  'eleven ',
  'twelve ',
  'thirteen ',
  'fourteen ',
  'fifteen ',
  'sixteen ',
  'seventeen ',
  'eighteen ',
  'nineteen '
];

// print the number n as a tring of words
// if sayAnd is true, then precede the string with 'and '
// if sayZero is true, then print 0 as 'zero', otherwise don't print anything
function sayNumber(n, sayAnd, sayZero) {
  var num = n;
  var result = '';
  
  // special case: n == 0
  if (num===0) {
    if(sayZero) {
      return 'zero'
    }
    else {
      return ''
    }
  }
  // deal with negatives
  if ( num < 0 ) {
    result += 'minus ';
    num *= -1;
  }
  // special case: n == +/- 1,000,000,000
  if ( num === 1000000000 ) {
    result += 'one billion';
  }
  else {
    if (sayAnd) {
      result += 'and ';
    }
    if (num >= 1000000) {
      // result = 'blah_blah million blip_blip'
      // we need to insert an 'and' if blip_blip is < 100
      result += sayNumber(Math.floor(num/1000000), false, false) + 'million ' + sayNumber(num % 1000000, (num % 1000000) < 100, false);
    }
    else if (num >= 1000) {
      // result = 'bloo_bloo thousand blat_blat'
      // we need to insert an 'and' if blat_blat is < 100
      result += sayNumber(Math.floor(num/1000), false, false) + 'thousand ' + sayNumber(num % 1000, (num % 1000) < 100, false);
    }
    else if (num >= 100) {
      // result = 'barf_barf hundred and bum_bum'
      // we always insert an 'and' except when bum_bum is 0
      result += sayNumber(Math.floor(num/100), false, false) + 'hundred ' + sayNumber(num % 100, (num % 100) !== 0, false);
    }
    else {
      if (num >= 90) {
        result += 'ninety ' + sayNumber(num % 10, false, false);
      }
      else if (num >= 80) {
        result += 'eighty ' + sayNumber(num % 10, false, false);
      }
      else if (num >= 70) {
        result += 'seventy ' + sayNumber(num % 10, false, false);
      }
      else if (num >= 60) {
        result += 'sixty ' + sayNumber(num % 10, false, false);
      }
      else if (num >= 50) {
        result += 'fifty ' + sayNumber(num % 10, false, false);
      }
      else if (num >= 40) {
        result += 'forty ' + sayNumber(num % 10, false, false);
      }
      else if (num >= 30) {
        result += 'thirty ' + sayNumber(num % 10, false, false);
      }
      else if (num >= 20) {
        result += 'twenty ' + sayNumber(num % 10, false, false);
      }
      else {
        result += unit[num];
      }
    }
  }
  return result;
}

var buttonClick = function() {
  var res = evaluate(document.getElementById('number-string').value);
  console.log(res);
  document.getElementById('result').innerText = res.toLocaleString();
}

var stringify = function() {
  var val = document.getElementById('number-digits').value
  var n = parseInt(val, 10);
  if (isNaN(n)) {
    document.getElementById('result-string').innerText = 'Error'
  }
  else {
    var res = sayNumber(n, false, true);
    document.getElementById('result-string').innerText = res.toLocaleString();
  }
}
