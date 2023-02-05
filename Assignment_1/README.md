# COL226 Assignment 1 Design Decisions
## Strings to Lists
Since the assignment specification called for a recursive program on lists, all strings were converted to lists before performing operations

## Addition of lists
To add long 2 strings, we can encode them as lists of integers, reverse them, adds their heads, and return the result. For instance, "3456" + "52" would be written as [6,5,4,3] + [2,5]. The actual addition takes place likes this:.  
__Step 1:__ 6+2=8, carry =0, pass addLists the tail of both the lists. Return 8::addLists(tl l1, tl l2) .  
__Step 2:__ 5+5=10, carry =1, pass addLists the tail of both the lists. Return 0::addLists(tl l1, tl l2) .  
__Step 3:__ list 2 is null, add carry to corresponding element of list 1. 4+1=5, carry =0 . Return 5::addLists(tl l1, tl l2) .  
__Step 4:__ list 2 is null, add carry to corresponding element of list 1. 3+0=3, carry=0. Return 3::addLists(tl l1, tl l2) .  
__Step 5:__ list 2 is null, list 1 is null, carry is 0. Return [] .  
__Final answer:__ Returns [8, 0, 5, 6]. Reversing this gives [6,5,0,8], the answer we needed. .  

## Subtraction of Lists
In a similar fashion, we describe the subtraction of lists. We deterministically pass a bigger number as big list, and then subtract the smaller number from it. We compare these lists using a custom comparator.

## Comparing lists
To compare 2 lists, we remove the zeroes in front of their most significant digits. We then compare their lengths, the list with the larger length is greater. Otherwise, we compare them in order of their most significant digits, and the one with the larger MSD at the first point of difference is the larger digit.

## Join lists
This concatenates two lists.


## findDigit and calcroot are explained in the proof of correctness

