// TODO handle expired token!

import axios, { AxiosError, AxiosResponse } from 'axios'
import { authToken } from './auth'
import { isPlainObject } from 'lodash'

export interface DetailedApiError {
  code: string
  message: string
}

export const apiClient = axios.create({
  baseURL: '/api',
  headers: {
    Authorization: `Bearer ${authToken.restore()}`,
  },
})

apiClient.interceptors.response.use(
  res => {
    return res
  },
  async (error: unknown) => {
    if (error instanceof AxiosError) {
      const data = error.response?.data

      if (getIsDetailedApiError(data)) {
        console.error('DetailedApiError', data)
        throw data
      }
    }

    // TODO handle 5xx globally

    throw error
  },
)

export function getIsDetailedApiError(
  data: AxiosResponse['data'] | undefined,
): data is DetailedApiError {
  if (isPlainObject(data)) {
    const { code, message } = data
    return typeof code === 'string' && typeof message === 'string'
  }

  return false
}
