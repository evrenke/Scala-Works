/*
PART 1:
Implementation of pipe and opt
 */
def pipe(reg: String => Boolean, reg2: String => Boolean): String => Boolean =
{
  def pipeHelper(s : String): Boolean = {
    reg(s) || reg2(s)
  }
  pipeHelper
}

def opt(reg: String => Boolean): String => Boolean = {
  def optHelper(s : String): Boolean = {
    reg(s) || s.equals("")
  }
  optHelper
}

/*
PART 2:
Implementation of follows
 */
def follows(reg: String => Boolean, reg2: String => Boolean): String => Boolean = {
{
  def followsHelper(s : String): Boolean = {
    var returned = false
    var index = 0
    while(index <= s.length) {
      if(reg(s.take(index)) && reg2(s.drop(index))) {
        returned = true
      }
      index = index + 1
    }
    returned
  }
  followsHelper
}

/*
PART 3:
Implementation of rep
 */
}
def rep(reg: String => Boolean) : String => Boolean = {
  def repHelper(s : String): Boolean = {
    var returned = false
    if(s.equals(""))
      returned = true
    else {
      var index = 0
      while(index <= s.length) { //"0000"
        if(reg(s.take(index)) && rep(reg)(s.drop(index))) {
          returned = true
        }
        index = index + 1
      }
    }
    returned
  }
  repHelper
}


// some basic recgnizers:
def s00(s: String) = s == "00"
def s111(s: String) = s == "111"

// exp recognizes the pattern (00)* ~ (111) ~ (00)?
def exp = follows(rep(s00), follows(s111, opt(s00)))

pipe(s00,s111)("00") // true
pipe(s00,s111)("111") // true
pipe(s00,s111)("") // false

opt(s00)("")//true
opt(s00)("0")//false
opt(s00)("00")//true

follows(s00,s111)("00111")// true
follows(s00,s111)("0011")// false

rep(s00)("") // true
rep(s00)("00") // true
rep(s00)("0000") // true
rep(s00)("00000") // false

exp("00111") // true
exp("00000011100") // = true
exp("000000111") // = true
exp("111") // = true
exp("00011100") // = false, why?