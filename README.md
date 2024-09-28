# Análisis de la Estructura Interna del IPIP-50

Este repositorio contiene un análisis exhaustivo de la estructura interna de la escala IPIP (International Personality Item Pool) de 50 ítems, utilizado para evaluar la personalidad en cinco dimensiones.

## Contenidos

1. **Revisión Sistemática**
   - Análisis de artículos académicos que reportan el uso de la escala IPIP-50.
   
2. **Descriptivos de la Muestra**
   - Estadísticas descriptivas relevantes de la muestra utilizada en el análisis.

3. **Pruebas de Idoneidad**
   - Verificación de supuestos previos:
     - Pruebas de colinealidad.
     - Pruebas de normalidad.
     - Medida de Adecuación Muestral (MSA).
     - Índice KMO (Kaiser-Meyer-Olkin).

4. **Análisis de Dimensionalidad**
   - Evaluación de la dimensionalidad de los datos:
     - Análisis paralelo por componentes principales.
     - Análisis de redes psicométricas con el paquete **EGAnet**.

5. **Análisis Factorial Exploratorio (AFE)**
   - Exploración inicial de la estructura factorial del IPIP-50.

6. **Análisis Factorial Confirmatorio (AFC)**
   - Evaluación de diferentes modelos factoriales:
     - Modelos simples.
     - Modelos jerárquicos.
     - Modelos bifactor.
   - Reporte de índices de ajuste para cada modelo.

7. **Invarianza**
   - Análisis de invarianza y su comparación con modelos de invarianza parcial.

8. **Fiabilidad**
   - Evaluación de la consistencia interna de la escala, tanto a nivel general como para cada subescala.

9. **Análisis de Ítems Problemáticos**
   - Revisión de ítems que presentan bajo rendimiento o dificultades psicométricas.

10. **Análisis de Patrones Aberrantes**
    - Detección de patrones aberrantes mediante el índice **Lz**.

11. **Propuestas de Mejora Teórica**
    - Sugerencias teóricas y prácticas para mejorar los modelos de personalidad basados en la escala IPIP-50.

## Herramientas Utilizadas

- **R** para el análisis estadístico y psicométrico.
- Paquetes utilizados: `psych`, `lavaan`, `EGAnet`, `parallel`, `semTools`, entre otros.
