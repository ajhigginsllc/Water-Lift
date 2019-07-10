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
retuned_queue <- new.env()
retuned_queue$front <- new.env()
retuned_queue$front$q <- NULL
retuned_queue$front$prev <- NULL
retuned_queue$last <- retuned_queue$front
print (c("ls() in new.queue", ls()))
return(retuned_queue)
}


## add to end of queue
enqueue <- function(queueIn, add){
queueIn$last$q <- new.env()
queueIn$last$q$prev <- queueIn$last
queueIn$last <- queueIn$last$q
queueIn$last$val <- add
queueIn$last$q <- NULL
# print (c("in enqueue, using env_print", queueIn$last$q))
# env_print(queueIn$last$q)
# print (c("in enqueue, using env_parents"))
# print (env_parents(queueIn$last$q))
}

## return front of queue and remove it
dequeue <- function(queueIn){
if (is.empty(queueIn)) {
	stop("Attempting to take element from empty queue")
	}
value <- queueIn$front$q$val
queueIn$front <- queueIn$front$q
queueIn$front$q$prev <- NULL
return(value)
}

## check to see if queue is empty
is.empty <- function(queueIn){
return(is.null(queueIn$front$q))
}

## walk the queue and print out all values
#
travelQueue <- function(queueIn, n) {
if (n == 0) {
	return
	} else {
		n <- n - 1
		print(queueIn$val)
		travelQueue(queueIn$q, n)
	}
}

}

N = 10
qq <- new.queue()
for(i in 1:N){
	enqueue(qq,i)
}
travelQueue(qq$front$q,N)

#while (! is.empty(qq)) {
#	print(dequeue(qq))
#}

