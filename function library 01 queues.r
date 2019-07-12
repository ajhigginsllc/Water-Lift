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
returned_queue <- new.env()
returned_queue$front <- new.env()
returned_queue$front$follow <- NULL
returned_queue$front$prev <- NULL
returned_queue$last <- returned_queue$front
return(returned_queue)
}


## add to end of queue
enqueue <- function(queueIn, add){
queueIn$last$follow <- new.env()
queueIn$last$follow$prev <- queueIn$last
queueIn$last <- queueIn$last$follow
queueIn$last$val <- as.numeric(add)
queueIn$last$follow <- NULL

}

forwardEnqueueAtTimet <- function (queueIn, funcTime) {
	print (c("entering forwardEnqueueAtTimet", funcTime, (funcTime >= queueIn$val), (funcTime <= queueIn$follow$val), str(funcTime), str(queueIn$val), str(queueIn$follow$val)))
#
#  need cases where adding to front of queue, when (funcTime <= queueIn$val), and to end of queue, when funcTime > queueIn$follow$val
#
	if (funcTime >= queueIn$val & funcTime <= queueIn$follow$val )  {
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
	stop("Attempting to take element from empty queue")
	}
value <- queueIn$front$follow$val
queueIn$front <- queueIn$front$follow
queueIn$front$follow$prev <- NULL
return(value)
}

## check to see if queue is empty
is.empty <- function(queueIn){
return(is.null(queueIn$front$follow))
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
	enqueue(qq,i)
	i <- i + 1
}
N <- 7

traverseQueue(qq$front$follow, N)
reverseTraverseQueue (qq$last, N)

success <- forwardEnqueueAtTimet (qq$front$follow, .7)

traverseQueue(qq$front$follow, N)
reverseTraverseQueue (qq$last, N)

#while (! is.empty(qq)) {
#	print(dequeue(qq))
#}

