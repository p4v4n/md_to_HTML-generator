# Heading 1 _is_ a [google link](http://www.google.com).

# Heading 2 is a [another google link][id1].

## Sub-heading ##

*************************

### Another ~~deeper~~ heading

Different Heading
 ================

![image1][path1]

Paragraphs [are][id1] separated\
by a blank line.

Text attributes _italic_, *italic*, __bold__, **bold**.

    (defn fact [n]
     (if (<= n 1)
       1
       (* (fact (- n 1)) n)))
 Another paragraph with some code `(defn sum [& args] (reduce + args))` in it.

>Want to become a better programmer? Sign up at [geekskool](http://www.geekskool.com).

![image2][path1]

Bullet list:

  * apples
  * oranges
  * pears

Numbered list:

  1. apples
  2. oranges
  3. pears

A ![space](resources/bootstrap.gif).

[id1]:http://google.com/ "title"
![path1]:resources/12friedman-master768-v4.gif