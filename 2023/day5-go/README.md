# Day 5

## Part 1

Part 1 is straightforward (see `fs1` function).

## Part 2

At first, I came up with a brute-force solution to go over each range of seeds. I leveraged concurrency to get a somewhat decent performance of ~14s (see `fs2NonFinal` function).

Then, I added a counter to see the range of numbers I was checking. I realized this number was about 10 times bigger than the final solution. Therefore, I came up with the reverse solution (see `fs2` function):

- Iterating from 0 to `math.MaxInt` for the location variable
- Going reverse (from location to humidity, etc.) until I get the seed value
- Then, I check whether the seed is among the provided ranges using a binary search
- If yes, I return the location

This version takes about 1.75s to complete.