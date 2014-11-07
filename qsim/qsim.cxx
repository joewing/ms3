
#include <iostream>
#include <jsoncpp/json/json.h>

#include "Simulator.hh"

int main(int argc, char *argv[])
{

    std::string str;
    std::getline(std::cin, str, '\0');

    Json::Reader reader;
    Json::Value root;
    if(!reader.parse(str, root)) {
        std::cerr << "error: could not parse JSON\n";
        return -1;
    }

    Simulator sim;

    // Parse the input.
    /* Input JSON format:
        {
            "bram_count": bram_count,
            "queues": [
                { "word_size": word_size,
                  "pdata": [ ... ],
                  "cdata": [ ... ] }, ...
            ]
        }
     */
    const uint32_t bram_count = root["bram_count"].asUInt();
    const Json::Value queues = root["queues"];
    const Json::ArrayIndex size = queues.size();
    for(Json::ArrayIndex i = 0; i < size; i++) {
        const Json::Value queue = queues[i];
        const uint32_t word_size = queue["word_size"].asUInt();
        std::vector<uint32_t> pdata, cdata;
        const Json::Value pvalues = queue["pdata"];
        const Json::ArrayIndex pcount = pvalues.size();
        for(Json::ArrayIndex j = 0; j < pcount; j++) {
            pdata.push_back(pvalues[j].asUInt());
        }
        const Json::Value cvalues = queue["cdata"];
        const Json::ArrayIndex ccount = cvalues.size();
        for(Json::ArrayIndex j = 0; j < ccount; j++) {
            cdata.push_back(cvalues[j].asUInt());
        }
        sim.AddQueue(word_size, pdata, cdata);
    }

    // Perform the simulation.
    const uint64_t t = sim.Run(bram_count);

    // Output the results.
    /* Output JSON format:
        {
            "total": total,
            "depths": [ depth, ... ]
        }
     */
    std::cout << "{\"total\": " << t << ",\n";
    std::cout << "\"depths\": [";
    std::vector<uint32_t> depths = sim.GetDepths();
    for(size_t i = 0; i < depths.size(); i++) {
        std::cout << depths[i];
        if(i + 1 < depths.size()) {
            std::cout << ", ";
        }
    }
    std::cout << "]}\n";

    return 0;
}
