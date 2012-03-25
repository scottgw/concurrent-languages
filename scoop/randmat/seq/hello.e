note
  description : "Root for trivial system printing a message"
  author : "Elizabeth W. Brown"

class HELLO create
  make

feature
  make
    -- Print a simple message.
  do
    io.put_string ("Hello World ")
    io.put_new_line
  end

end -- class HELLO
