module Main

import clonedetectors::Type1CloneDetector;
import clonedetectors::Type2CloneDetector;
import Prelude;
import utils::Utils;
import lang::java::jdt::m3::Core;
import lang::java::jdt::m3::AST;
import util::Math;


/*
* compute metrics such as code duplication, biggest clone etc. for type 1 and 2 clones
*/
public map[int, tuple[num,num,num,num]] getCloneMetrics(map[int, set[set[tuple[loc,value]]]] clones, loc project){
	map[int, tuple[num,num,num,num]] cloneMetrics = ();	

	setPrecision(2);
	model = createM3FromEclipseProject(project);
	totalLOCs = calculateVolume(model);
	
	for( i <- [1..3]){		
		num biggestCloneClass = 0;
		tuple[num, num] cloneMetrics1 = <0,0>;
		if(size(clones[i]) > 0){
			biggestCloneClass = max([size(c) | c <- clones[i]]);
			cloneMetrics1 = calculateClonesMetrics(clones[i], model);
		}
		dupLOCsPerc = cloneMetrics1[0]/totalLOCs * 100;
		cloneMetrics[i] = <dupLOCsPerc, size(clones[i]), cloneMetrics1[1], biggestCloneClass>;
	}
	
	return cloneMetrics;
}

/*
* prepare all the necessary data and generate an output json used for the visualization module
*/
private str genOutputJson(map[int, set[set[tuple[loc,value]]]] clones, loc project) {
	
	//map[int, list[tuple[str,str]]] cloneExamples = getCloneExamples(clones);
	map[int, tuple[num,num,num,num]] cloneMetrics = getCloneMetrics(clones, project);	
	
	/*
	* - create a map so that for each file we have a set of all the clones in that file
	* - for each clone in the file we store its location and the corresponding clones
	*/
	map[str, set[tuple[loc, set[loc], int]]] files = ();
	map[str, int] fileLines = ();
	
	for(cloneType <- clones)
		for(cloneClass <- clones[cloneType])
			for(clone <- cloneClass){
				if (clone[0].file notin files){
					files[clone[0].file] = {};
					fileLines[clone[0].file] = size(readFileLines(toLocation(clone[0].uri)));
				}
				set[loc] clonePairs = {};
				for( c <- cloneClass)
					if( c != clone)
						clonePairs += {c[0]};
				files[clone[0].file] += {<clone[0], clonePairs, cloneType>};
			}
	
	/*
	* generate the output json based on the previously created map
	*/		
	str json = "{";
	json += "\"cloneMetrics\" : [";
	for(i <- [1..3]){
		json +=	"{";
		json += "\"cloneType\" : " + toString(i) + "," ;
		json += "\"duplicationPercentage\" : " + toString(cloneMetrics[i][0]) + ",";
		json += "\"noOfCloneClasses\" : " + toString(cloneMetrics[i][1]) + ",";
		json += "\"biggestClone\" : " + toString(cloneMetrics[i][2]) + ",";
		json += "\"biggestCloneClass\" : " + toString(cloneMetrics[i][3]); // + ",";
		json += "}";
		if(i == 1)
			json += ",";
	}	
	json += "],";
	json += "\"files\":[";
	bool firstFile = true;
	for(file <- files){
		if(!firstFile)
			json += ",";
		firstFile = false;
		json += "{";
		json += "\"file\": \"" + file + "\",";
		json += "\"lines\": " + toString(fileLines[file]) + ",";
		json += "\"clones\": [";
		bool firstClone = true;
		for(cl <- files[file]){
			if(!firstClone)
				json += ",";
			firstClone = false;
			json += "{";
			json += "\"cloneType\": " + toString(cl[2]) + ",";
			json += "\"lineStart\": " + toString(cl[0].begin.line) + ",";
			json += "\"lineEnd\": " + toString(cl[0].end.line) + ",";
			str description = "";
			for (c <- cl[1]){
				description += c.file + " (Line " + toString(c.begin.line) + " to " + toString(c.end.line) +");";
			}
			json += "\"description\": \"" + description + "\"";
			json += "}";
		}
		json += "]";
		json += "}";		
	}
	
	json += "]}";
	//println(json);
	
	loc jsonFile = |project://SoftwareEvolutionWDC2/src/json.txt|;
	writeFile(jsonFile, json);
	return json;
}

public void writeClonesToFile(map[int, set[set[tuple[loc,value]]]] clones, loc project){
	loc clonesFile = |project://SoftwareEvolutionWDC2/src/clones.txt|;
	writeFile(clonesFile, "");
	for(cloneType <- clones){
		str cloneTypeMsg = "Type <cloneType> clones:\n\n\n";
		appendToFile(clonesFile, cloneTypeMsg);
		for(cloneClass <- clones[cloneType]){
			appendToFile(clonesFile, "Clone class\n");
			for(clone <- cloneClass){
				appendToFile(clonesFile, "File: <clone[0].file>\nClone source code:\n<readFile(clone[0])>\n\n");
			}
		}
	}
}

public void main(loc project) {
	
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
		
	/*
	* Type 1 clones
	*/
	println("Indentifying type 1 clones...");
	clonesT1 = calculateClonesT1(ast);
	println("Done!");
	
	/*
	*Type 2 clones
	*/
	println("Identifying type 2 clones...");
	clonesT2 = calculateClonesT2(ast, 2);
	println("Done");
	
	println("Generating json for visualization...");
	map[int, set[set[tuple[loc,value]]]] allClones = ();
	allClones[1] = clonesT1;
	allClones[2] = clonesT2;
	genOutputJson(allClones, project);
	println("Done!");
	
	println("Writing clones to file...");
	writeClonesToFile(allClones, project);
	println("Done!");
}