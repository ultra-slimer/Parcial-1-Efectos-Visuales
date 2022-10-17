using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ShieldScript : MonoBehaviour
{

    public Vector3 mousePos;
    public GameObject shieldGameObject;
    Camera cam;
    public LayerMask mask;

    bool isOn = true;

    // Start is called before the first frame update
    void Start()
    {
        cam = Camera.main;
    }

    // Update is called once per frame
    void Update()
    {
        mousePos = Input.mousePosition;

        if (Input.GetMouseButtonDown(0))
        {
            mousePos = Input.mousePosition;
            mousePos.z = 100f;
            mousePos = cam.ScreenToWorldPoint(mousePos);

            Ray ray = cam.ScreenPointToRay(Input.mousePosition);
            RaycastHit hit;

            if(Physics.Raycast(ray,out hit, 100,mask.value))
            {
                shieldGameObject.GetComponent<Renderer>().material.SetVector("_CollisionPoint", hit.point);
                print("hit");
                shieldGameObject.GetComponent<Animation>().Play();
            }
        }

        if (Input.GetKeyDown(KeyCode.Space))
        {
            if (!isOn)
            {
                isOn = true;
                shieldGameObject.GetComponent<Animation>().Play("ShieldOn");
            }
            else
            {
                isOn = false;
                shieldGameObject.GetComponent<Animation>().Play("ShieldOff");
            }
        }
    }



    private void OnDrawGizmos()
    {
        Gizmos.DrawWireSphere(mousePos, 1f);
    }
}
