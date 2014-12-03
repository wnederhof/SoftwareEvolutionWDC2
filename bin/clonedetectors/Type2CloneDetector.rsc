module clonedetectors::Type2CloneDetector

public void calculateClones(loc project) {
	set[Declaration] AST = createAstsFromEclipseProject(project, true);
	
	/*visit (AST) {
	case \newArray a => 
	}*/

	return Type2CloneDetector::calculateClonesAST(AST);
}