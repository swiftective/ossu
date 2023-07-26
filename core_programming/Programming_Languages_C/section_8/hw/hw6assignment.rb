# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here

  # class method to choose the next piece
  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece(board)
    MyPiece.new(MyCheatPiece, board)
  end

  MyCheatPiece = [[[0, 0]]]

  # class array holding all the pieces and their rotations
  All_My_Pieces = All_Pieces + [rotations([[0, 0], [1, 0], [0, -1]]),
                                rotations([[0, 0], [0, 1], [1, 0], [1, 1], [2, 1]]),
                                [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]],
                                 [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]]]
end

class MyBoard < Board
  # your enhancements here
  def initialize(game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheat = false
  end

  # gets the next piece
  def next_piece
    if @cheat
      @current_block = MyPiece.cheat_piece(self)
      @cheat = false
      @score -= 100
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def cheat
    if not @cheat and @score >= 100
      @cheat = true
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here

  def key_bindings
    super
    @root.bind('u', proc { 2.times { @board.rotate_clockwise } })
    @root.bind('c', proc { @board.cheat })
  end

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
end
