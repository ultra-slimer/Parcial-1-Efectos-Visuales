using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PortalScript : MonoBehaviour
{
    bool isOn = true;

    // Update is called once per frame
    void Update()
    {
        if (Input.GetKeyDown(KeyCode.Space))
        {
            if (!isOn)
            {
                isOn = true;
                GetComponent<Animation>().Play("PortalOn");
            }
            else
            {
                isOn = false;
                GetComponent<Animation>().Play("PortalOff");
            }
        }
    }
}
