module Buffer where

-- Type class for data structures that can represent the text buffer
-- of an editor.

class Buffer b where

  -- | Convert a buffer to a String.
  toString :: b -> String

  -- | Create a buffer from a String.
  fromString :: String -> b

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line :: Int -> b -> Maybe String

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine :: Int -> String -> b -> b

  -- | Compute the number of lines in the buffer.
  numLines :: b -> Int

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value :: b -> Int
