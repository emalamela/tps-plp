Object subclass: #Direction
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Direction commentStamp: '<historical>' prior: 0!
Direction!


!Direction methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:52'!
nextFrom: puntoDeOrigen
"Dado un punto, devuelve el resultado de moverse en la direccion correspondiente. "
^self class nextFrom: puntoDeOrigen ! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Direction class
	instanceVariableNames: ''!

!Direction class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:54'!
nextFrom: puntoDeOrigen
	^ self subclassResponsibility ! !


!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:59'!
southwest
	^Southwest new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:53'!
north
	^North new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:52'!
south
	^South new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:59'!
northeast
	^Northeast new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:53'!
east
	^East new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:53'!
west
	^West new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:59'!
northwest
	^Northwest new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:59'!
southeast
	^Southeast new! !


Direction subclass: #East
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!East commentStamp: '<historical>' prior: 0!
East!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

East class
	instanceVariableNames: ''!

!East class methodsFor: 'evaluating' stamp: 'PLP 10/14/2017 23:32'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x + 1)@(puntoDeOrigen y)! !


Object subclass: #Game
	instanceVariableNames: 'terrain gameTeams currentTeamIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Game commentStamp: 'EmanuelLamela 11/9/2017 15:43' prior: 0!
I represent the state of a Game!


!Game methodsFor: 'handling' stamp: 'EmanuelLamela 11/27/2017 23:24'!
pass
	"Pass the turn to the next team"
	currentTeamIndex := currentTeamIndex \\ (gameTeams size) + 1! !


!Game methodsFor: 'private ' stamp: 'EmanuelLamela 11/27/2017 23:07'!
initializeWithTerrain: aTerrain
	currentTeamIndex := 0.
	terrain := aTerrain.! !


!Game methodsFor: 'as yet unclassified' stamp: 'JuanCruz 11/8/2017 01:23'!
currentTeam
	^gameTeams at: currentTeamIndex! !

!Game methodsFor: 'as yet unclassified' stamp: 'JuanCruz 11/8/2017 03:14'!
moveCharacter: anInteger to: aDirection steps: anInteger2
	| gameCharacter |
	gameCharacter := self currentTeam members at: anInteger.
	gameCharacter move: anInteger2 to: aDirection.
	self pass! !

!Game methodsFor: 'as yet unclassified' stamp: 'EmanuelLamela 11/29/2017 14:20'!
registerTeam: aTeam playerLocations: locationToCharacterPairs
	"Register a team to participate in the game"
	| characters |
	characters := locationToCharacterPairs pairsCollect: [
		:location :characterClass | characterClass value new: terrain with: location and: aTeam
	].
	gameTeams ifNil: [ gameTeams := OrderedCollection new. currentTeamIndex := 1 ].
	gameTeams add: (GameTeam new: aTeam with: characters).! !

!Game methodsFor: 'as yet unclassified' stamp: 'EmanuelLamela 11/27/2017 22:57'!
currentWinners
	| teamsPoints winnersTeams winnersScore |
	teamsPoints := gameTeams
		groupedBy: [ :gameTeam | (terrain selectFlagsByTeam: gameTeam) size ].
	winnersTeams := OrderedCollection new.
	winnersScore := teamsPoints keys max.
	winnersScore > 0
		ifTrue: [ winnersTeams := teamsPoints at: winnersScore ].
	^ winnersTeams collect: [ :winnerTeam | winnerTeam name ]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Game class
	instanceVariableNames: 'currentTeamIndex gameTeams terrain'!

!Game class methodsFor: 'initialization' stamp: 'EmanuelLamela 11/27/2017 23:08'!
newX: anInteger Y: anInteger2 trapLocations: tCollection flagLocations: fCollection
	| aTerrain |
	aTerrain := Terrain dimX: anInteger dimY: anInteger2.
	tCollection do: [:trapLocation|aTerrain at: trapLocation put: Trap new].
	fCollection do: [:flagLocation|aTerrain at: flagLocation put: Flag new].
	^ self new initializeWithTerrain: aTerrain.! !


Object subclass: #GameCharacter
	instanceVariableNames: 'terrain team canMove location'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!GameCharacter commentStamp: 'EmanuelLamela 11/9/2017 15:54' prior: 0!
I represent an abstract Game Character and its possible actions

Each type of character can perform certain actions when interacting with other entities!


!GameCharacter methodsFor: 'protocol' stamp: 'EmanuelLamela 11/29/2017 00:10'!
renable
	"Allows the character to perform its designated actions"
	canMove := true.! !

!GameCharacter methodsFor: 'protocol' stamp: 'EmanuelLamela 11/29/2017 00:10'!
disable
	"Prevents character for performing any actions"
	canMove := false.! !


!GameCharacter methodsFor: 'initialization' stamp: 'EmanuelLamela 11/29/2017 00:19'!
initializeWithTeam: aTeamName andTerrain: aTerrain andLocation: aPointInTerrain
	team := aTeamName.
	location := aPointInTerrain.
	terrain := (aTerrain at: aPointInTerrain put: self).
	canMove := true.
	! !



!GameCharacter methodsFor: 'accessing' stamp: 'EmanuelLamela 11/27/2017 23:28'!
isObjective: aMapObject
	self subclassResponsibility! !

!GameCharacter methodsFor: 'accessing' stamp: 'JuanCruz 11/4/2017 16:54'!
team
	^team! !

!GameCharacter methodsFor: 'accessing' stamp: 'EmanuelLamela 11/27/2017 23:28'!
action
	self subclassResponsibility! !


!GameCharacter methodsFor: 'path commands' stamp: 'EmanuelLamela 11/9/2017 15:50'!
moveTo: aDirection
"Moves the character 1 position towards aDirection if the terrain allows it"
| content |
(self canMoveTo: aDirection) ifFalse: [ ^self].
terrain remove: self.
location := aDirection nextFrom: location.
content := terrain at: location.
terrain at: location put: self.
content ifNil: [self interactWithAllElements] ifNotNil: [ self disable ].
^self! !


!GameCharacter methodsFor: 'movement' stamp: 'EmanuelLamela 11/29/2017 00:07'!
canMove
	^ canMove.! !

!GameCharacter methodsFor: 'movement' stamp: 'EmanuelLamela 11/28/2017 00:21'!
canMoveTo: aDirection 
	| movingPosition |
	movingPosition := terrain getPositionFrom: self movingTo: aDirection.
	^ canMove and: ((terrain contains: movingPosition) and: [ | objectAtMovingPosition | objectAtMovingPosition := (terrain at: movingPosition). objectAtMovingPosition isNil or: [objectAtMovingPosition isVisible not] ] )! !


!GameCharacter methodsFor: 'as yet unclassified' stamp: 'EmanuelLamela 11/29/2017 00:54'!
healedBy: aGameCharacter
	aGameCharacter heal: self! !

!GameCharacter methodsFor: 'as yet unclassified' stamp: 'EmanuelLamela 11/27/2017 23:38'!
selectSeenFrom: aCollection
	^ aCollection select: [:aMapObject | (aMapObject isVisible)]! !

!GameCharacter methodsFor: 'as yet unclassified' stamp: 'JuanCruz 11/7/2017 20:23'!
location
	^ location! !

!GameCharacter methodsFor: 'as yet unclassified' stamp: 'EmanuelLamela 11/9/2017 15:51'!
move: anInteger to: aDirection
	"Moves the character in aDirection a number of times as long as terrain allows it"
	anInteger timesRepeat: [ self moveTo: aDirection ]! !

!GameCharacter methodsFor: 'as yet unclassified' stamp: 'EmanuelLamela 11/27/2017 23:26'!
interactWithAllElements
	(terrain elementsAround: self satisfying: self target) collect: self action! !


!GameCharacter methodsFor: 'testing' stamp: 'JuanCruz 11/7/2017 19:03'!
target
	^[:object | self isObjective: object]! !

!GameCharacter methodsFor: 'testing' stamp: 'EmanuelLamela 11/29/2017 00:06'!
canHeal
	self subclassResponsibility.! !

!GameCharacter methodsFor: 'testing' stamp: 'JuanCruz 11/4/2017 19:31'!
isVisible
	^true! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

GameCharacter class
	instanceVariableNames: 'canMove'!

!GameCharacter class methodsFor: 'creation' stamp: 'EmanuelLamela 11/29/2017 00:23'!
new: aTerrain with: aPoint and: aTeamName
	^ self new initializeWithTeam: aTeamName andTerrain: aTerrain andLocation: aPoint.! !


GameCharacter subclass: #Engineer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Engineer commentStamp: 'EmanuelLamela 11/9/2017 15:52' prior: 0!
I represent an Engineer character which can remove traps!


!Engineer methodsFor: 'as yet unclassified' stamp: 'EmanuelLamela 11/27/2017 23:40'!
selectSeenFrom: aCollection
	^ aCollection! !

!Engineer methodsFor: 'as yet unclassified' stamp: 'JuanCruz 11/7/2017 19:54'!
action
	^[:aTrap | terrain remove: aTrap ]! !


!Engineer methodsFor: 'testing' stamp: 'JuanCruz 11/7/2017 19:52'!
isObjective: aMapObject
	^aMapObject isKindOf: Trap! !


!Engineer methodsFor: 'accessing' stamp: 'EmanuelLamela 11/29/2017 00:07'!
canHeal
	^ false! !




GameCharacter subclass: #Explorer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Explorer commentStamp: 'EmanuelLamela 11/9/2017 15:53' prior: 0!
I represent an Explorer character which can claim a Flag!



!Explorer methodsFor: 'accessing' stamp: 'EmanuelLamela 11/29/2017 00:07'!
canHeal
	^ false! !


!Explorer methodsFor: 'testing' stamp: 'JuanCruz 11/7/2017 19:52'!
isObjective: aMapObject
	^aMapObject isKindOf: Flag! !



!Explorer methodsFor: 'as yet unclassified' stamp: 'JuanCruz 11/7/2017 19:57'!
action
	^[:aFlag | aFlag team: self team] ! !


Object subclass: #GameTeam
	instanceVariableNames: 'team members'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!GameTeam commentStamp: 'EmanuelLamela 11/9/2017 15:57' prior: 0!
I represent a team that can participate in a Game

The Game Team is identified through a name!


!GameTeam methodsFor: 'accessing' stamp: 'EmanuelLamela 11/29/2017 00:56'!
team
	^ team! !

!GameTeam methodsFor: 'accessing' stamp: 'EmanuelLamela 11/29/2017 00:58'!
name
	^ team! !

!GameTeam methodsFor: 'accessing' stamp: 'JuanCruz 11/7/2017 20:40'!
members
	^ members! !


!GameTeam methodsFor: 'initialization' stamp: 'EmanuelLamela 11/29/2017 00:43'!
initializeWithTeam: aTeamName andMembers: teamMembersCollection
	team := aTeamName.
	members := teamMembersCollection.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

GameTeam class
	instanceVariableNames: ''!

!GameTeam class methodsFor: 'initialization' stamp: 'EmanuelLamela 11/29/2017 00:44'!
new: aTeamName with: members
	^ self new initializeWithTeam: aTeamName andMembers: members! !


PackageManifest subclass: #ManifestPLP
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!ManifestPLP commentStamp: '<historical>' prior: 0!
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ManifestPLP class
	instanceVariableNames: ''!

!ManifestPLP class methodsFor: 'meta-data' stamp: 'EmanuelLamela 11/9/2017 15:51'!
description ^ 'I represent an Engineer character which can remove traps'! !


Object subclass: #MapObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!MapObject commentStamp: 'EmanuelLamela 11/9/2017 16:01' prior: 0!
I represent an abstract item which can be placed on a Game map

Different Map Objects can hold a visible object or none!


!MapObject methodsFor: 'evaluation' stamp: 'EmanuelLamela 11/29/2017 00:45'!
isVisible 
	"Describes whether it's visible"
	self subclassResponsibility ! !

!MapObject methodsFor: 'evaluation' stamp: 'EmanuelLamela 11/29/2017 00:45'!
value
	self subclassResponsibility! !

!MapObject methodsFor: 'evaluation' stamp: 'EmanuelLamela 11/29/2017 00:45'!
getVisibleObject
	self subclassResponsibility! !


MapObject subclass: #Flag
	instanceVariableNames: 'team'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Flag commentStamp: 'EmanuelLamela 11/9/2017 16:02' prior: 0!
I represent a Flag object that can be claimed by 1 Game Team at a time!


!Flag methodsFor: 'evaluation' stamp: 'JuanCruz 11/4/2017 12:46'!
team: aString
	team := aString.! !

!Flag methodsFor: 'evaluation' stamp: 'EmanuelLamela 11/29/2017 00:46'!
getVisibleObject
	^ self.! !

!Flag methodsFor: 'evaluation' stamp: 'JuanCruz 11/4/2017 12:47'!
team
	^ team! !

!Flag methodsFor: 'evaluation' stamp: 'EmanuelLamela 11/29/2017 00:47'!
isVisible
	^ true.! !

!Flag methodsFor: 'evaluation' stamp: 'EmanuelLamela 11/29/2017 00:47'!
value
	^ self getVisibleObject.! !


GameCharacter subclass: #Medic
	instanceVariableNames: 'canPerformHeal'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Medic commentStamp: 'EmanuelLamela 11/9/2017 15:55' prior: 0!
I represent a Medic character which can heal other Game Characters!


!Medic methodsFor: 'protocol' stamp: 'EmanuelLamela 11/29/2017 00:30'!
renable
	canPerformHeal := true.
	super renable.! !

!Medic methodsFor: 'protocol' stamp: 'EmanuelLamela 11/29/2017 00:30'!
disable
	canPerformHeal := false.
	super disable.! !


!Medic methodsFor: 'testing' stamp: 'JuanCruz 11/7/2017 19:24'!
isObjective: aMapObject
	^(aMapObject isKindOf: GameCharacter) and: ([(aMapObject canMove) not and: (self team = aMapObject team)])! !


!Medic methodsFor: 'initialization' stamp: 'EmanuelLamela 11/29/2017 00:36'!
initializeWithTeam: aTeamName andTerrain: aTerrain andLocation: aPointInTerrain
	canPerformHeal := true.
	super initializeWithTeam: aTeamName andTerrain: aTerrain andLocation: aPointInTerrain.! !


!Medic methodsFor: 'action' stamp: 'EmanuelLamela 11/29/2017 00:51'!
heal: aGameCharacter
	self assert: self canHeal.
	aGameCharacter renable.! !


!Medic methodsFor: 'accessing' stamp: 'EmanuelLamela 11/29/2017 00:56'!
action
	^ [:aTeammate | self heal: aTeammate].! !

!Medic methodsFor: 'accessing' stamp: 'EmanuelLamela 11/29/2017 00:28'!
canHeal
	^ canPerformHeal! !


Direction subclass: #North
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!North commentStamp: '<historical>' prior: 0!
North!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

North class
	instanceVariableNames: ''!

!North class methodsFor: 'evaluating' stamp: 'PLP 10/14/2017 23:33'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x)@(puntoDeOrigen y + 1)! !


Direction subclass: #Northeast
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Northeast commentStamp: '<historical>' prior: 0!
Northeast!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Northeast class
	instanceVariableNames: ''!

!Northeast class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:57'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x + 1)@(puntoDeOrigen y + 1)! !


Direction subclass: #Northwest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Northwest commentStamp: '<historical>' prior: 0!
Northwest!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Northwest class
	instanceVariableNames: ''!

!Northwest class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:57'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x - 1)@(puntoDeOrigen y + 1)! !


Direction subclass: #South
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!South commentStamp: '<historical>' prior: 0!
South!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

South class
	instanceVariableNames: ''!

!South class methodsFor: 'evaluating' stamp: 'PLP 10/14/2017 23:33'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x)@(puntoDeOrigen y - 1)! !


Direction subclass: #Southeast
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Southeast commentStamp: '<historical>' prior: 0!
Southeast!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Southeast class
	instanceVariableNames: ''!

!Southeast class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:58'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x + 1)@(puntoDeOrigen y - 1)! !


Direction subclass: #Southwest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Southwest commentStamp: '<historical>' prior: 0!
Southwest!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Southwest class
	instanceVariableNames: ''!

!Southwest class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:58'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x - 1)@(puntoDeOrigen y - 1)! !


Object subclass: #Terrain
	instanceVariableNames: 'dimX dimY mapObjects'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Terrain commentStamp: 'EmanuelLamela 11/9/2017 16:05' prior: 0!
I represent the terrain of the Game which delimits the playable area and over which different Map Objects can be placed!


!Terrain methodsFor: 'accessing' stamp: 'EmanuelLamela 11/9/2017 15:23'!
getPositionFrom: aObject
	^mapObjects keyAtValue: aObject ifAbsent: nil.! !

!Terrain methodsFor: 'accessing' stamp: 'JuanCruz 11/4/2017 14:30'!
getDimX
	^dimX! !

!Terrain methodsFor: 'accessing' stamp: 'EmanuelLamela 11/9/2017 16:08'!
elementsAround: aGameCharacter
	"Visible objects in the adyacents positions to aGameCharacter"
	| neighborsPositions neighborsObjects |
	neighborsPositions := (self getPositionFrom: aGameCharacter)
		eightNeighbors.
	neighborsObjects := neighborsPositions
		collect: [ :neighborPosition | self at: neighborPosition ].
	^ aGameCharacter
		selectSeenFrom: (neighborsObjects reject: [ :neighborObject | neighborObject isNil ])! !

!Terrain methodsFor: 'accessing' stamp: 'JuanCruz 11/7/2017 20:23'!
getPositionFrom: aObject movingTo: aDirection
	| aPosition |
	aPosition := self getPositionFrom: aObject .
	^ aDirection nextFrom: aPosition.! !

!Terrain methodsFor: 'accessing' stamp: 'JuanCruz 11/4/2017 14:30'!
getDimY
	^dimY! !

!Terrain methodsFor: 'accessing' stamp: 'EmanuelLamela 11/9/2017 16:09'!
elementsAround: aGameCharacter satisfying: aBlock
	"Visible objects in the adyacents positions to aGameCharacter that satisfy a condition"
	^(self elementsAround: aGameCharacter) select: aBlock.! !

!Terrain methodsFor: 'accessing' stamp: 'EmanuelLamela 11/27/2017 23:56'!
at: aPoint
	^ mapObjects at: aPoint ifAbsent: nil! !


!Terrain methodsFor: 'comparing' stamp: 'EmanuelLamela 11/9/2017 16:06'!
contains: aPoint 
	"Whether the values of aPoint fit in the dimensions of the terrain"
	^aPoint <= ((self getDimX)@(self getDimY)) and:[aPoint > (0@0)].! !


!Terrain methodsFor: 'inserting' stamp: 'EmanuelLamela 11/29/2017 00:21'!
at: aPoint put: aMapObject
	"Puts a MapObject in aPoint if the said point is contained in the terrain"
	self assert: (self contains: aPoint).
	mapObjects at: aPoint put: aMapObject.! !


!Terrain methodsFor: 'removing' stamp: 'EmanuelLamela 11/9/2017 16:11'!
remove: aMapObject
	"Removes aMapObject if present in the terrain"
 	mapObjects removeKey: ( self getPositionFrom: aMapObject ) ifAbsent: nil! !


!Terrain methodsFor: 'setting' stamp: 'EmanuelLamela 11/29/2017 01:07'!
initialize: dimensionX and: dimensionY
	mapObjects := Dictionary new.
	dimX := dimensionX.
	dimY := dimensionY.! !


!Terrain methodsFor: 'viewing' stamp: 'EmanuelLamela 11/9/2017 16:12'!
visibleAt: aPoint
	"Returns the visible object on the map if any is present, else nil"
	| aMapObject |
	aMapObject := mapObjects at: aPoint ifAbsent: nil.
	^aMapObject ifNil: nil ifNotNil: [aMapObject getVisibleObject]! !


!Terrain methodsFor: 'enumerating' stamp: 'EmanuelLamela 11/27/2017 22:54'!
select: aBlock
	^ mapObjects select: aBlock ! !

!Terrain methodsFor: 'enumerating' stamp: 'EmanuelLamela 11/27/2017 22:55'!
selectFlagsByTeam: aGameTeam
	^ self select: [ :mapObject | (mapObject isKindOf: Flag) and: [ mapObject team = aGameTeam name ] ]
		! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Terrain class
	instanceVariableNames: 'dimX dimY'!

!Terrain class methodsFor: 'creation' stamp: 'EmanuelLamela 11/29/2017 01:10'!
dimX: aDimX dimY: aDimY
	^ self new initialize: aDimX and: aDimY.! !

MapObject subclass: #Trap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Trap commentStamp: 'EmanuelLamela 11/9/2017 16:03' prior: 0!
I represent a Trap object that disables Game Characters that step over them!


!Trap methodsFor: 'evaluation' stamp: 'JuanCruz 11/4/2017 12:44'!
isVisible
	^false.! !

!Trap methodsFor: 'evaluation' stamp: 'JuanCruz 11/4/2017 12:36'!
value
	^nil.! !

!Trap methodsFor: 'evaluation' stamp: 'JuanCruz 11/4/2017 16:44'!
getVisibleObject
	^nil.! !


Direction subclass: #West
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!West commentStamp: '<historical>' prior: 0!
West!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

West class
	instanceVariableNames: ''!

!West class methodsFor: 'evaluating' stamp: 'PLP 10/14/2017 23:34'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x - 1)@(puntoDeOrigen y)! !
