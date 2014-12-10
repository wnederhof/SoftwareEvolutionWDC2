module utils::Utils

import lang::java::jdt::m3::Core;
import IO;
import String;
import Prelude;
import lang::java::jdt::m3::AST;

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

/*
* Compute the size of a statement
*/
public num countStatements(Statement impl){
	num result = 0;
		
		visit (impl){			
			case \switch(_,_): result += 1;
			case \if(_, _):	result += 1;
			case \if(_, _, _): result += 2;
			case \while(_, _) : result +=1;
			case \do(_, _) : result +=2;
			case \foreach(_, _, _) : result +=1;
			case \for(_, _, _) : result += 1;
			case \for(_, _, _, _) : result +=1;
			case \try(_, list[Statement] catchClauses): result += size(catchClauses) + 1;
			case \try(_, list[Statement] catchClauses, _) : result += size(catchClauses) + 2;
			case \break(): result += 1;
		    case \break(_): result += 1;
		    case \continue(): result += 1;
		    case \continue(_): result += 1;
		    case \return(_): result += 1;
		    case \return(): result += 1;
		    case \case(_): result += 1;
		    case \defaultCase(): result += 1;
		    case \synchronizedStatement(_,_): result += 1;
		    case \throw(_): result += 1;
		    case \declarationStatement(_): result += 1;
		    case \expressionStatement(_): result += 1;
		    case \assert(_): result += 1;
		    case \assert(_,_): result += 1;
		}
	//println(result);
	return result;
}