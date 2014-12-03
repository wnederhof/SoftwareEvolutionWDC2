module clonedetectors::Type1CloneDetector

/*
 * How the clone detector works:
 *
 * First, we read in all methods. We believe that only methods
 * matter in code duplication, because for example imports
 * may be exactly the same for example for Swing classes,
 * simply because these use the same structure.
 *
 * 
 */
import Prelude;
import lang::java::jdt::m3::AST;

private int calcMass(value s) {
	return 6;// size(s@src.length - s@src.offset);
}

public int calculateClonesAST(AST, threshold) {
	list[tuple[tuple[loc,value], tuple[loc,value]]] clones = [];
	
	// A map of string to a set of locations.
	map[value, loc] bucket = ();
	
	for (Declaration d <- AST) {
		visit (d) {
		case Declaration s:
			if (calcMass(s) > threshold) {
				if (!s in bucket) {
					bucket[s] = {};
				}
				bucket[s] += s@src;
			}
		case Statement s:
			if (calcMass(s) > threshold) {
				if (!s in bucket) {
					bucket[s] = {};
				}
				bucket[s] += s@src;
			}
		}
	}
	
	return 0;
}

public int calculateClones(loc project) {
	set[Declaration] AST = createAstsFromEclipseProject(project, true);
	return calculateClonesAST(AST, 6);
}