# Use library(rlang) for environmental functions

# current_env() is the environment in which code is currently executing


# You can find the parent of an environment with env_parent()

# You can see all ancestors with env_parents()


# env_parents(current_env(), last = empty_env()) to get all the way par global, but you include all packages

# In base R, Use parent.env() to find the parent of an environment. No base function returns all ancestors.


# Unlike lists, setting an element to NULL does not remove it, because sometimes you want a name that refers to NULL. Instead, use env_unbind():


#  [[To remove an item from a list, set it to null?]]


#It’s natural to work with environments recursively:
#f <- function(..., env = caller_env()) {
#if (identical(env, empty_env())) {
# base case
#} else if (success) {
# success case
#} else {
# recursive case
#f(..., env = env_parent(env))
#} 
#}
# Each package attached by library() or require() becomes one of the parents of the global environment. The immediate parent of the global environment is the last package you attached31, the parent of that package is the second to last package you attached, …If you follow all the parents back, you see the order in which every package has been attached. This is known as the search path because all objects in these environments can be found from the top-level interactive workspace. You can see the names of these environments with base::search(), or the environments themselves with rlang::search_envs():


# A function binds the current environment when it is created. This is called the function environment, and is used for lexical scoping. 


# Across computer languages, functions that capture (or enclose) their environments are called closures, which is why this term is often used interchangeably with function in R’s documentation.


# You can get the function environment with fn_env():
# y <- 1
# f <- function(x) x + y
# fn_env(f)
#> <environment: R_GlobalEnv>
# environment(f)  in base R




{
new.queue <- function() {
returned_queue <- new.env()   						# let's label this new environment as e1
returned_queue$head <- new.env()					# e2
returned_queue$tail <- new.env()					# e3, but it's always at the end of the queue
returned_queue$follow <- returned_queue$head		# e1-> follow is assigned to point to e2
returned_queue$follow$follow <- returned_queue$head	# e2 -> follow points forward to e3
returned_queue$tail$prev <- returned_queue$head		# e3 points back to e2
returned_queue$head$follow <- returned_queue$tail	# e1->e2->follow is assigned and points to e3
returned_queue$head$prev <- NULL					# e1->e2-> prev is assigned NULL
returned_queue$tail$follow <- NULL					# e1->e3->follow is assigned NULL - nothing follows the end
returned_queue$head$val <- 0						# queue implementation is designed for all positive values.  The "null" header environment always has a value of zero.
returned_queue$tail$val <- .Machine$integer.max		# tail of the queue is always the maximum "val" that the queue can hold
return(returned_queue)
}

forwardEnqueueAtTimet <- function (queueIn, funcTime) {
	if (funcTime >= queueIn$val & funcTime <= queueIn$follow$val )  {		# assumes this queue has a "null" or zero dummy header environment and a "max" tail environment
		print (c("adding time", funcTime, "between", queueIn$val, "and", queueIn$follow$val))
		oldFollow <- queueIn$follow
		oldPrev <- queueIn$follow$prev
		queueIn$follow <- new.env()
		queueIn$follow$prev <- oldPrev
		oldPrev$follow <- queueIn$follow
		oldFollow$prev <- queueIn$follow
		queueIn$follow$follow <- oldFollow
		queueIn$follow$val <- as.numeric(funcTime)
		return (TRUE)
		} else {
			print (c("continuing to search for queue spot", funcTime, queueIn$val, queueIn$follow$val))
			forwardEnqueueAtTimet (queueIn$follow, funcTime)
			}
	}

## return front of queue and remove it
dequeue <- function(queueIn){
	if (is.empty(queueIn)) {
		stop("Attempting to take element from empty queue")			# this would mean that the sim has created no additional time-value entries to process
		}
	value <- queueIn$head$follow$val
	queueIn$head$follow$prev <- queueIn$head
	queueIn$head$follow <- queueIn$head$follow$follow
	return(value)
}

## check to see if queue is empty.... the min/max dummy values (environments) at the head and tail of the queue need to be ignored and always kept intact
## queue is empty when the head == tail
#
is.empty <- function(queueIn){
	if (identical(queueIn$tail, queueIn$head$follow)) {
		return (TRUE)
	} else {
		return (FALSE) 
		}
}

## walk the queue and print out all values
#
traverseQueue <- function(queueIn, n) {
if (n == 0) {
	return
	} else {
		n <- n - 1
#		print(c("prev=", queueIn$prev, "val=", queueIn$val, "follow=", queueIn$follow))
		print (queueIn$val)
		traverseQueue(queueIn$follow, n)
	}
}

reverseTraverseQueue <- function(queueIn, n) {
	if (n == 0) {
		return
	} else {
		n <- n-1
#		print (c("prev=", queueIn$prev, "val=", queueIn$val, "follow=", queueIn$follow))
		print (queueIn$val)
		reverseTraverseQueue (queueIn$prev, n)
		}
	}

}

N = 8
qq <- new.queue()
i <- 1.1
while (i < N) {
#	enqueue(qq,i)
	forwardEnqueueAtTimet (qq$head, i)					# start recursion pointing to e2 (qq points to e1, e1$head points to e2
	i <- i + 1
}
N <- 7

traverseQueue(qq$head$follow, N)
reverseTraverseQueue (qq$tail$prev, N)

success <- forwardEnqueueAtTimet (qq$head, .7)
success <- forwardEnqueueAtTimet (qq$head, 3.7)
success <- forwardEnqueueAtTimet (qq$head, 5.7)
success <- forwardEnqueueAtTimet (qq$head, 13.7)

N <- N + 4

traverseQueue(qq$head$follow, N)
reverseTraverseQueue (qq$tail$prev, N)

#while (! is.empty(qq)) {
#	print(dequeue(qq$head$follow))
#}

