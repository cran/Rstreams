This is the source for an R package for reading 
binary files.  So far I haven't been able to compile it,
so you can't use library() to load it:  you need to manually
execute

  source('Rstreams\\R\\Rstreams.R')
  dyn.load('Rstreams\\src\\Rstreams.dll')

After doing this, you can open a file using

  s _ openstream('somefile')

and read from it using readint(), readfloat(), etc.  

Duncan Murdoch