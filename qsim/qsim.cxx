
#include <iostream>
#include <jsoncpp/json/json.h>
#include <cassert>

#include "Simulator.hh"
#include "Parser.hh"

/** Parse the input.
 * Input JSON format:
 *  {
 *      "bram_count": bram_count,
 *      "kernels": [
 *          { "type": type, ... }, ...
 *      ],
 *      "queues": [
 *          { "id": id,
 *            "word_size": word_size }, ...
 *      ]
 *  }
 */
static bool ParseInput(Simulator *sim)
{
    Parser parser(sim);
    return parser.Run();
}

/** Output results.
 * Output JSON format:
 *  {
 *      "total": total,
 *      "queues": [
 *          { "id": id, "depth": depth }, ...
 *      ]
 *  }
 */
static void OutputResults(const Simulator &sim, const uint64_t t)
{
    std::cout << "{\"total\": " << t << ",\n";
    std::cout << "\"queues\": [";
    std::vector<std::pair<uint32_t, uint32_t> > depths = sim.GetDepths();
    for(size_t i = 0; i < depths.size(); i++) {
        const std::pair<uint32_t, uint32_t> p = depths[i];
        std::cout << "{\"id\": " << p.first << ", ";
        std::cout << "\"depth\": " << p.second << "}";
        if(i + 1 < depths.size()) {
            std::cout << ", ";
        }
    }
    std::cout << "]}\n";
}

int main(int argc, char *argv[])
{

    // Parse input.
    Simulator sim;
    if(!ParseInput(&sim)) {
        return -1;
    }

    // Perform the simulation.
    const uint64_t t = sim.Run();

    // Output the results.
    OutputResults(sim, t);

    return 0;
}
