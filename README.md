<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline1">1. Haskell Tic Tac Toe implementation with minimax solution</a></li>
</ul>
</div>
</div>

# Haskell Tic Tac Toe implementation with minimax solution<a id="orgheadline1"></a>

    ghc tic.hs
    ./tic

    +-+-+-+
    | | | |
    +-+-+-+
    | | | |
    +-+-+-+
    | | |X|
    +-+-+-+
    
    +-+-+-+
    | | | |
    +-+-+-+
    | |O| |
    +-+-+-+
    | | |X|
    +-+-+-+
    
    +-+-+-+
    | | | |
    +-+-+-+
    | |O| |
    +-+-+-+
    | |X|X|
    +-+-+-+
    
    +-+-+-+
    | | | |
    +-+-+-+
    | |O| |
    +-+-+-+
    |O|X|X|
    +-+-+-+
    
    +-+-+-+
    | | |X|
    +-+-+-+
    | |O| |
    +-+-+-+
    |O|X|X|
    +-+-+-+
    
    +-+-+-+
    | | |X|
    +-+-+-+
    | |O|O|
    +-+-+-+
    |O|X|X|
    +-+-+-+
    
    +-+-+-+
    | | |X|
    +-+-+-+
    |X|O|O|
    +-+-+-+
    |O|X|X|
    +-+-+-+
    
    +-+-+-+
    | |O|X|
    +-+-+-+
    |X|O|O|
    +-+-+-+
    |O|X|X|
    +-+-+-+
    
    +-+-+-+
    |X|O|X|
    +-+-+-+
    |X|O|O|
    +-+-+-+
    |O|X|X|
    +-+-+-+