module utils::LocCalculator

import lang::java::jdt::m3::Core;
import IO;
import String;
import Prelude;

/*
* replace comments by white spaces
*/
public str replaceByWhiteSpaces (str s, int offset, int length) {
	str sN = substring (s, 0, offset);
	for (i <- [0..length])
		sN += " ";
	return sN + substring (s, offset + length); 
}


/*
* calculate the size of a block given its location, offset, length and model
*/
public int calcBlockSize (loc l, M3 model) {
	int offset;
	try {
		offset = l.offset;
	} catch UnavailableInformation():
		offset = 0;		
	str file = readFile(l);
	for (d <- [<d[1].offset, d[1].length> | d <- model@documentation, (d[1].path == l.path || d[0].path == l.path)]) {
		try {
			file = replaceByWhiteSpaces(file, d[0] - offset, d[1]);
		} catch:
			continue;
	}
	return size([li | str li <- split("\n", file), size(trim(li)) != 0]);
}

/*
* calculate the volume of a compilation unit
*/
public int calcCompilationUnitVol(loc l, M3 model) {
	str s = readFile(l);
	set[loc] locs = {l[1] | l <- model@documentation, l[0].path == cu.path || l[1].path == cu.path};
	for (l2 <- locs) {
		s = replaceByWhiteSpaces (s, l2.offset, l2.length);
	}
	linesOfCode = size([lineOfCode | lineOfCode <- split("\n", s), size(trim(lineOfCode)) != 0]);
	return linesOfCode;
}
 
/*
* calculate the volume of a project
*/ 
public num calculateVolume(M3 model) {
	compilationUnits = {l[0] | l <- model@containment, isCompilationUnit(l[0])};
	return sum([calcCompilationUnitVol(l, model) | l <- compilationUnits]);
}

/*
* calculate the size of the clones
*/
public tuple[num, num] calculateClonesMetrics(set[set[tuple[loc,value]]] clones, M3 model) {
	list[num] cloneSizes = [];
	for(clone <- clones){
		for (c <- clone)
			cloneSizes += [calcBlockSize(c[0], model)];
	}
	return <sum(cloneSizes), max(cloneSizes)>;
}