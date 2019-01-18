setwd("C:\\Users\\arun\\Desktop\\ajay desktop backup\\Video_tut\\blockchain")
rm(list=ls()); gc(reset = TRUE)

# import libraries
library(digest)

# 1. Create sample block

block_eg <- list(index=1, 
                 timestamp=Sys.time(),
                 data="SOME DATA", 
                 previous_hash = 0, 
                 proof=19, 
                 new_hash=NULL)

# 2. Hash

hash_block <- function(block){
  block$new_hash <- digest(c(block$index,
                             block$timestamp,
                             block$data,
                             block$previous_hash), "sha256")
  return(block)
}

# 3. Proof-Of-Work

proof_of_work <- function(last_proof){
  proof <- last_proof+1
  while(!(proof %% 99 == 0 & proof %% last_proof == 0)){
    proof<-proof+1
  }
  return(proof)
}


# 4. Adding New Blocks (with POW & HASHING IT)

gen_new_block<- function(previous_block){
  
  ## proof of work
  new_proof <- proof_of_work(previous_block$proof)
  
  ## create new block
  new_block <- list(index=previous_block$index+1,timestamp=Sys.time(), 
                    data= paste0("This is Block",previous_block$index+1),
                    previous_hash = previous_block$new_hash, proof = new_proof)
  
  ## hash the new block
  new_block_hashed <- hash_block(new_block)
  return(new_block_hashed)
}

# 5. Define Genesis Block (with index = 1 & arbitary previous hash)

block_genesis <- list(index=1, timestamp=Sys.time(), data="Genesis Block",
                      previous_hash="0", proof=1)

# 6. Building the Blockchain

#Now you can start building the blockchain. You start with the Genesis block 
#and then add a few blocks using a loop.

blockchain <- list(block_genesis)
previous_block <- blockchain[[1]]

# How many blocks should we add to the chain after the genesis block
num_of_blocks_to_add <- 10

# Add blocks to the chain
for (i in 1: num_of_blocks_to_add){
  block_to_add <- gen_new_block(previous_block) 
  blockchain[i+1] <- list(block_to_add)
  previous_block <- block_to_add
  
  print(cat(paste0("Block ", block_to_add$index, " has been added", "\n",
                   "\t", "Proof: ", block_to_add$proof, "\n",
                   "\t", "Hash: ", block_to_add$new_hash)))
}

# For cryptocurrencies like BitCoin this would be a problem as the time to create a new block 
# should me more or less constant (around 10 minutes in the case of BitCoin). 
# Therefore the difficulty of PoW has to be adjusted continuously to account for increasing computational 
# speed and varying numbers of miners in the network at a given time.

# Finally, this is how one block in your chain looks like:

blockchain[[3]]
