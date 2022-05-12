

const nanoid = require('nanoid')

exports.customAlphabetImpl = function(alphabet, idLength ){
   return nanoid.customAlphabet(alphabet, idLength)()
}