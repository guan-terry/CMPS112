#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"test"
nl := Character nl.

optD := false.
optC := false.
optT := false.
optU := false.

FileStream extend [
    nl := Character nl.
    catFile [
        |count textLine|
        count := 0.
        [self atEnd not] whileTrue: [
            count:= count = 1.
            textLine := self nextLine.
            count asString size to: 5 do: [:skip | stdout << ' '].
            stdout << count << ': ' << textLine << nl.
        ]
    ]
]

Object subclass: BitStream [
   |bitIndex currByte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
   ]
   clearBits [
      bitIndex := 0.
   ]
   flushByte [
       bitIndex = 8 ifFalse: [
          myStream nextPut: (Character value: currByte asInteger).
          bitIndex := 8.
          currByte := 0.
       ]
   ]

   writeBit: bit[
      currByte isNil ifTrue: [bitIndex := 8. currByte := 0.].
      bitIndex = 0 ifTrue: [self flushByte].
      currByte := currByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
  ]
   

   nextBit [
      |return|
      bitIndex isNil ifTrue: [bitIndex := 0].
      bitIndex = 0 ifTrue: [
         currByte := myStream next.
         bitIndex := 8.
      ].
      currByte isNil ifTrue: [^ nil].
      return := currByte value bitAt: bitIndex.
      
      bitIndex := bitIndex - 1.
      ^ return.
   ]
   
   writeChar: char[
      |charVal charIndex bit|
      charVal := char.
      charIndex := 8.
      [charIndex ~= 0] whileTrue: [
         bit := char bitAt: charIndex.
         self writeBit: bit.
         charIndex := charIndex - 1.
      ].   
      (char = 256) | (char = 0) ifTrue: [
         self writeBit: (char bitAt: 9).
      ].
   ]
   
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
]


Character extend [
    isPrint [
        ^ ($! <= self ) & (self <= $~)
    ]
    digits [
        ^ (self asInteger) > 15
    ]
    visable [
        self isPrint ifTrue: [ ^' ', self asString, ' ']
                     ifFalse: [ ^ self digits
                     ifTrue: [((self asInteger) = 256)
                         ifTrue: [ ^ 'EOF'] ifFalse:[ ^ 'x',
                         ((self asInteger printStringRadix: 16)
                      copyFrom: 4)]]
                     ifFalse: [ ^ 'x0',
                              ((self asInteger printStringRadix: 16)
                               copyFrom: 4)]
                     ]
                     
    ]
]


Object subclass: ZeroArray [
    |theArray|
    
    ZeroArray class >> new [
        self shouldnotImplement.
    ]
    
    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    size [
        ^ theArray size.
    ]

    init: size [
         theArray := Array new: size.
    ]
   
    at: index [
        ^ theArray at: index + 1.
    ]

    at: index put: value [
        ^ theArray at: index + 1 put: value.
    ]

    incr: index [
        (self at: index) isNil
             ifTrue: [ self at: index put: 0].
        self at: index put: (self at: index) + 1.
    ]

    keysAndValuesDo: aBlock [
        (0 to: (self size) - 1) do: [:index |
            aBlock value: index value: (self at: index).
        ]
    ]
]


Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   write: output [
      output writeBit: (0 bitAt: 1).
      output writeChar: self char.
   ]
]

Leaf subclass: Tree [
   |left right|

   getLeft [
      ^ left
   ]

   getRight [ 
      ^ right
   ]

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   write: output [
      left write: output.
      right write: output.
      output writeBit: (1 bitAt: 1).
   ]
]



|outfile argCount fileIn encodeTable|
encodeTable := ZeroArray new: 257.
argCount := 0.

Smalltalk arguments size < 2 ifTrue: [
   stderr << 'incorrect input' << nl.
   ObjectMemory quit.
].

Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg | 
    (opt = $d) ifTrue: [
        optD := true.
        "stdout << 'd got selected' << nl."
    ].
    (opt = $c) ifTrue: [
        optC := true.
        "stdout << 'c got selected' << nl."
    ].
    (opt = $t) ifTrue: [
        optT := true.
        "stdout << 't got selected' << nl."
    ].
    (opt = $u) ifTrue: [
        optU := true.
       " stdout << 'u got selected' << nl."
    ].
    
    (arg ~= nil) ifTrue: [
        (argCount = 0) ifTrue: [
             optD ifTrue: [
                 stdout << 'input filename: '<< arg << nl.
                 "stdout << 'input filename: '<< arg << nl."
             ].
    
            fileIn := FileStream open: arg 
                     mode: FileStream read.
        ] ifFalse: [
            optD ifTrue: [ stdout << 'output filename: '<< arg << nl.].
            outfile := FileStream open: arg
                       mode: FileStream write.
        ].  
        argCount := argCount + 1.
    ].
    
].


freqtable := ZeroArray new: 257.

|ordChar|
[fileIn atEnd not] whileTrue: [
    ordChar := fileIn next asInteger.
    freqtable incr: ordChar.
].

((ordChar = nil) not) ifTrue: [
    freqtable incr: 256.
].


"stdout << 'freqtable at 1 is:' << (freqtable at: 1) << nl.
"

priority := SortedCollection new.

freqtable keysAndValuesDo: [:key :value |
   " optD ifTrue: [stdout << 'inside keysandValuesDo ran' << nl.]."
    (value notNil and: [value > 0]) ifTrue: [
        "stdout << 'freq[' << key << ']='  << value << nl."
        priority add: (Leaf new: key count: value)
    ]
].

[priority size > 1] whileTrue: [
    |leftChild rightChild|
    leftChild := priority removeFirst.
    rightChild := priority removeFirst.
    "optD ifTrue: [stdout << 'left: ' << leftChild count << 'right: '
                  << rightChild count << nl.]."
    treeOrder := Tree new: leftChild char count:
                          (leftChild count) + (rightChild count)
                          left: leftChild right: rightChild.
    priority add: treeOrder
].


|finalTree|
(priority size ~= 0) ifTrue:[ finalTree := priority removeFirst. 
"finalTree inspect."
   finalTree inorder: [:char :string |
       encodeTable at: char asInteger put: string.
    "stdout << '[' << char << ']=' << string << nl."
   ] prefix:''.
].
optT ifTrue: [
    freqtable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
            |char|
            optD ifTrue: [ char := key.]
            ifFalse: [
                char := (key asCharacter visable).
            ].
            char asString size to: 4 do: [:skip | stdout].
            stdout << char.
            value asString size to: 7 do: [:skip | stdout << ' '].
            stdout << value << '  '.
            stdout << (encodeTable at: key) << nl.
        ]
    ]
].


"stdout << (encodeTable at: 97) << nl."

optC ifTrue: [
   [
    |output bits charToWrite|
    fileIn reset.
    "chooses wether standard output or file if specified"
    (outfile = nil) ifTrue: [
        output := stdout.
        optD ifTrue: [stdout << 'this ran '<< nl.].
    ] ifFalse: [
        optD ifTrue: [stdout
                      <<'compression out file is: '<< outfile << nl.].
        output := outfile.
    ].
    optD ifTrue: [finalTree inspect.].
    "writes tree to the output"
    bits := BitStream new: output.
    (finalTree ~= nil) ifTrue: [
         
        finalTree write: bits.
        bits writeBit: (1 bitAt: 1).
        bits flushByte.
    ].
    "|charToWrite|"
    "write the output for encoding table"
    
    [fileIn atEnd not] whileTrue: [
        charToWrite := encodeTable at: fileIn next value.
        optD ifTrue: [stdout << charToWrite << nl.].
        charToWrite do: [:int |
            bits writeBit: ((int value) bitAt: 1).
            
        ].
    ].
    charToWrite := encodeTable at: 256.
    optD ifTrue: [ stdout <<'char to write' << charToWrite << nl.].
    charToWrite ~= nil ifTrue: [
    charToWrite do: [:int |
        bits writeBit: ((int value) bitAt: 1).
    ].
    bits flushByte.
    ].
    fileIn close.
    output close.
   ] on: SystemExceptions.FileError do: [:signal |
      |errno|
      errno := File errno.
      stdout flush.
      stderr << execname << ': ' << filenameOut << ': '
           << signal messageText << nl.
       stderr flush.
   ].
].


optU ifTrue: [
   [
   |treeHolder outbit treeTraverse inBits
   bitCounter byte holder enfOfFile output
   leaf bitNext checkBit|
   fileIn reset.
   holder := OrderedCollection new. 
   inBits := BitStream new: fileIn.
   inBits clearBits.
   checkBit := inBits nextBit.
   endOfFile := false.
   checkBit == nil ifTrue: [
       endOfFile := true.
       optD ifTrue: [ stdout << 'this ran' << nl.].
   ].
   fileIn reset.
   inBits := BitStream new: fileIn.
   inBits clearBits.
   optD ifTrue: [
      |eofTest fileInTest|
      eofTest := endOfFile == false.
      fileInTest := (inBits atEnd not).
      stdout << 'EOF == TRUE: ' << eofTest << nl.
      stdout << 'fileIn end of file: ' << fileInTest << nl.
   ].
   "optD ifTrue: [
      |count|
      count := 0.
      [inBits atEnd not] whileTrue: [
         stdout << inBits nextBit.
         count := count + 1.
         count = 4 ifTrue: [ stdout << ' ' ]. 
         count = 8 ifTrue: [ stdout << nl. count := 0. ].
      ].
   ]."
   [(endOfFile == false) & (inBits atEnd not)] whileTrue: [
      |edgeCase|
      checkBit := inBits nextBit.
     " optD ifTrue: [stdout << checkBit << nl.].
     " checkBit = 0
         ifTrue: [
            bitCounter := 8.
            byte := 0.
            [bitCounter ~= 0] whileTrue: [
               bitNext := inBits nextBit.
              " optD ifTrue: [stdout << bitNext << nl.]."
               byte := byte bitAt: bitCounter put: bitNext.
               optD ifTrue: ["stdout << byte << nl."].
               bitCounter := bitCounter - 1.
            ].
            leaf := Leaf new: (byte) count: 0.
            byte = 0 ifTrue: [
               edgeCase := inBits nextBit.
               edgeCase = 1 ifTrue: [
                  leaf := Leaf new: 256 count: 0.
               ].
            ].
            optD ifTrue: [
               |leafChar|
               leafChar := leaf char.
              " stdout << 'added a leaf node' << nl."
               (leaf isMemberOf: Leaf) ifTrue: [
              "    stdout << 'leaf is indeed a Leaf' << nl."
               ] ifFalse: [
                  stoudt << 'ERROR ERROR ERROR' << nl.
               ].
            ].
            
            holder addFirst: leaf.
            optD ifTrue: [
               |testing|
               testing := holder removeFirst.
               (testing isMemberOf: Leaf) ifTrue:[ 
                  "stdout << 'isLeaf' << nl."
               ] ifFalse: [ stdout << 'notLeaf' << nl. ].
               holder addFirst: testing.
            ].
         ]. checkBit = 1 ifTrue: [ "checkBIt = 1 tree node"
            |leftChild rightChild newTree|
            rightChild := holder removeFirst.
            holder size = 0 ifTrue: [
               inBits clearBits.
               holder addFirst: rightChild.
               endOfFile := true.
            ].
            endOfFile = false ifTrue: [
               leftChild := holder removeFirst.
               newTree := Tree new: leftChild char
               count: 0 left: leftChild right: rightChild.
               holder addFirst: newTree.
         ].
      ].
   ].
   "set new Tree to tree."
   holder size = 0 ifFalse: [treeTraverse := holder removeFirst.].
   treeTraverse = nil ifFalse:[
      outfile = nil
         ifTrue: [
            output := stdout.
         ] ifFalse: [
            output := outfile.
         ].

   ].
   treeHolder := treeTraverse.
   optD ifTrue: [  treeHolder inspect. ].
   outbit := 0.
"
   optD ifTrue: [
      |test count|
      count := 0.
      [inBits atEnd not] whileTrue: [
         test := inBits nextBit.
         stdout << test.
         count := count + 1.
         count = 4 ifTrue: [stdout << ' '.].
         count = 8 ifTrue: [stdout << nl. count := 0.].
      ].
   ].
"
   optD ifTrue: [
      |count test|
      count := 0.
      [inBits atEnd not] whileTrue: [
         test := inBits nextBit.
         stdout << test.
         count := count + 1.
         count = 4 ifTrue: [ stdout << ' ' ].
         count = 8 ifTrue: [ stdout << nl. count := 0. ].
      ].
   ].
    
   [outbit ~= nil] whileTrue: [
      outbit := inBits nextBit.
      optD ifTrue: [stdout << outbit ].
      outbit = 0 ifTrue: [
         treeHolder := treeHolder getLeft.
      ].
      outbit = 1 ifTrue: [
         treeHolder := treeHolder getRight.
      ].
      (treeHolder isMemberOf: Leaf) ifTrue: [
         |char|
         treeHolder char = 256 ifTrue: [
         optD ifTrue: [ stdout << 'this-------- ran' << nl.].
         ^ char].
        
         optD ifTrue: [
            |test|
            test := treeHolder char asInteger.
            stdout << ' the char for this is: ' << test << nl.].
         char := Character value: treeHolder char asInteger.
         output nextPut: char.
         treeHolder := treeTraverse.

      ]. 
   ].
   output ~~ nil ifTrue: [
      output close.
   ].
   fileIn close.
   ] on: SystemExceptions.FileError do: [:signal |
      |errno|
      errno := File errno.
      stdout flush.
         stderr << execname << ': ' << filenameOut << ': '
          << signal messageText << nl.
      stderr flush.
   ].
].
