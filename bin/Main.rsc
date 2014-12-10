module Main

import clonedetectors::Type1CloneDetector;
import clonedetectors::Type2CloneDetector;
import Prelude;
import utils::LocCalculator;
import lang::java::jdt::m3::Core;
import util::Math;

//TODO somehow integrate in the map also the clone type
private str genOutputJson(map[int, set[set[tuple[loc,value]]]] clones) {
	
	/*
	* create a map so that for each file we have a set of all the clones in that file
	* for each clone in the file we store its location and the corresponding clones
	*/
	map[str, set[tuple[loc, set[loc], int]]] files = ();
	
	for(cloneType <- clones)
		for(cloneClass <- clones[cloneType])
			for(clone <- cloneClass){
				if (clone[0].file notin files)
					files[clone[0].file] = {};
				set[loc] clonePairs = {};
				for( c <- cloneClass)
					if( c != clone)
						clonePairs += {c[0]};
				files[clone[0].file] += {<clone[0], clonePairs, cloneType>};
			}
	
	/*
	* generate the output json based on the previously created map
	*/
		
	str json = "[{\"files\":[";
	for(file <- files){
		json += "{";
		json += "\"file\": \"" + file + "\",\n";
		json += "\"clones\": [";
		for(cl <- files[file]){
			json += "{";
			json += "\"cloneType\": \"" + toString(cl[2]) + "\",";
			json += "\"lineStart\": \"" + toString(cl[0].begin.line) + "\",";
			json += "\"lineEnd\": \"" + toString(cl[0].end.line) + "\",";
			str description = "";
			for (c <- cl[1]){
				description += c.file + " (Line " + toString(c.begin.line) + " to " + toString(c.end.line) +")";
			}
			json += "\"description\": \"" + description + "\"";
			json += "}\n";
		}
		json += "]";
		json += "}\n";		
	}
	
	println(json);
	return json;
}

public void main(loc project) {

	model = createM3FromEclipseProject(project);
	totalLOCs = calculateVolume(model);
	
	// TODO output example clones and number of clones (pairs for type 2)
	/*
	* Type 1 clones
	*/
	clonesT1 = calculateClonesT1(project);
	num biggestCloneClassT1 = 0;
	tuple[num, num] cloneMetricsT1 = <0,0>;
	if(size(clonesT1) > 0){
		biggestCloneClassT1 = max([size(c) | c <- clonesT1]);
		cloneMetricsT1 = calculateClonesMetrics(clonesT1, model);
	}
	dupLOCsPercT1 = cloneMetricsT1[0]/totalLOCs * 100;

	println("\nType 1 Clones
			 'Duplicated lines : <dupLOCsPercT1>%
	         'Number of clone classes : <size(clonesT1)>
	         'Biggest clone: <cloneMetricsT1[1]> LOCs
	         'Biggest clone class: <biggestCloneClassT1>
	         ");

	/*
	*Type 2 clones
	*/
	clonesT2 = calculateClonesT2(project, 2);
	num biggestCloneClassT2 = 0;
	tuple[num, num] cloneMetricsT2 = <0,0>;
	if(size(clonesT2) > 0){
		biggestCloneClassT2 = max([ size(c) | c <- clonesT2]);
		cloneMetricsT2 = calculateClonesMetrics(clonesT2, model);
	}
	dupLOCsPercT2 = cloneMetricsT2[0]/totalLOCs * 100;
	
	println("\nType 2 Clones
			 'Duplicated lines : <dupLOCsPercT2>%
	         'Number of clone classes : <size(clonesT2)>
	         'Biggest clone: <cloneMetricsT2[1]> LOCs
	         'Biggest clone class: <biggestCloneClassT2>
	         ");
	
	
	map[int, set[set[tuple[loc,value]]]] allClones = ();
	allClones[1] = clonesT1;
	allClones[2] = clonesT2;
	genOutputJson(allClones);
	
	/*int numberOfClones = 0;
	int biggestCloneClass = 0;
	
	for (c <- cloneClassesT2){
		
		int sizeCloneClass = size(c);
		
		//BiggestCloneClass
		if (size(c) > biggestCloneClass)
			biggestCloneClass = size(c);
			
		//Number of Clones
		numberOfClones += sizeCloneClass;
	}*/
	
	
	
	/*for (c <- clones) {
		println("clone class size <size(clones[c])>");
		for (clone <- clones[c]){
			println(clone[1]);
		}
	}*/
}